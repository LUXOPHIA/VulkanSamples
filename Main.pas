﻿unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  vulkan_core,
  vulkan.util, vulkan.util_init,
  LUX, LUX.Code.C,
  LUX.GPU.Vulkan;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
    const sample_title = 'Draw Textured Cube';
  public
    { public 宣言 }
    _Vulkan     :TVulkan;
    _Device     :TVkDevice;
    _Window     :TVkWindow;
    _ShaderVert :TVkShaderVert;
    _ShaderFrag :TVkShaderFrag;
    _Pipeline   :TVkPipeline;

    imageAcquiredSemaphore :VkSemaphore;
    drawFence              :VkFence;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses WinApi.Windows, WinApi.Messages,
     cube_data;

procedure run( var info:T_sample_info );
begin
     (* Placeholder for samples that want to show dynamic content *)
end;

function WndProc( hwnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM ) :LRESULT; stdcall;
var
   info :P_sample_info;
begin
     info := P_sample_info( GetWindowLongPtr( hWnd, GWLP_USERDATA ) );

     case uMsg of
       WM_CLOSE:
          PostQuitMessage( 0 );
       WM_PAINT:
          begin
               run( info^ );
               Exit( 0 );
          end;
     else
     end;
     Result := DefWindowProc( hWnd, uMsg, wParam, lParam );
end;

procedure TForm1.FormCreate(Sender: TObject);
const
     depthPresent :T_bool = True;
var
   res                              :VkResult;
   clear_values                     :array [ 0..2-1 ] of VkClearValue;
   imageAcquiredSemaphoreCreateInfo :VkSemaphoreCreateInfo;
   rp_begin                         :VkRenderPassBeginInfo;
   offsets                          :array [ 0..1-1 ] of VkDeviceSize;
   cmd_bufs                         :array [ 0..1-1 ] of VkCommandBuffer;
   fenceInfo                        :VkFenceCreateInfo;
   pipe_stage_flags                 :VkPipelineStageFlags;
   submit_info                      :array [ 0..1-1 ] of VkSubmitInfo;
   present                          :VkPresentInfoKHR;
begin
     _Vulkan := TVulkan.Create;

     _Vulkan.Instance := TVkInstance.Create( _Vulkan );
     _Window := TVkWindow.Create( _Vulkan.Instance.Devices.Devices[0], 500, 500, @WndProc );
     init_swapchain_extension( _Vulkan );
     init_command_pool( _Vulkan );
     init_command_buffer( _Vulkan );
     execute_begin_command_buffer( _Vulkan );
     init_device_queue( _Vulkan );
     init_swap_chain( _Vulkan );
     init_depth_buffer( _Vulkan );
     init_texture( _Vulkan );
     init_uniform_buffer( _Vulkan );
     init_descriptor_and_pipeline_layouts( _Vulkan, true );
     init_renderpass( _Vulkan, depthPresent );
     init_framebuffers( _Vulkan, depthPresent );
     init_vertex_buffer( _Vulkan, @g_vb_texture_Data[0], SizeOf( T_VertexUV ) * Length( g_vb_texture_Data ), SizeOf( T_VertexUV ), True );
     init_descriptor_pool( _Vulkan, True );
     init_descriptor_set( _Vulkan, True );
     init_pipeline_cache( _Vulkan );
     _Pipeline := TVkPipeline.Create( _Vulkan.Instance.Devices.Devices[0], depthPresent );
     _ShaderVert := TVkShaderVert.Create( _Pipeline );
     _ShaderFrag := TVkShaderFrag.Create( _Pipeline );
     _ShaderVert.LoadFromFile( '../../_DATA/draw_textured_cube.vert' );
     _ShaderFrag.LoadFromFile( '../../_DATA/draw_textured_cube.frag' );
     _Pipeline.CreateHandle;

     (* VULKAN_KEY_START *)

     clear_values[0].color.float32[0]     := 0.2;
     clear_values[0].color.float32[1]     := 0.2;
     clear_values[0].color.float32[2]     := 0.2;
     clear_values[0].color.float32[3]     := 0.2;
     clear_values[1].depthStencil.depth   := 1.0;
     clear_values[1].depthStencil.stencil := 0;

     imageAcquiredSemaphoreCreateInfo.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
     imageAcquiredSemaphoreCreateInfo.pNext := nil;
     imageAcquiredSemaphoreCreateInfo.flags := 0;

     res := vkCreateSemaphore(  _Vulkan.Instance.Devices.Devices[0].Handle, @imageAcquiredSemaphoreCreateInfo, nil, @imageAcquiredSemaphore );
     Assert( res = VK_SUCCESS );

     // Get the index of the next available swapchain image:
     res := vkAcquireNextImageKHR(  _Vulkan.Instance.Devices.Devices[0].Handle,  _Vulkan.Info.swap_chain, UINT64_MAX, imageAcquiredSemaphore, VK_NULL_HANDLE,
                                   @ _Vulkan.Info.current_buffer );
     // TODO: Deal with the VK_SUBOPTIMAL_KHR and VK_ERROR_OUT_OF_DATE_KHR
     // return codes
     Assert( res = VK_SUCCESS );

     rp_begin.sType                    := VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
     rp_begin.pNext                    := nil;
     rp_begin.renderPass               :=  _Vulkan.Info.render_pass;
     rp_begin.framebuffer              := _Vulkan.Info.framebuffers[_Vulkan.Info.current_buffer];
     rp_begin.renderArea.offset.x      := 0;
     rp_begin.renderArea.offset.y      := 0;
     rp_begin.renderArea.extent.width  := _Vulkan.Instance.Devices.Devices[0].Window.width;
     rp_begin.renderArea.extent.height := _Vulkan.Instance.Devices.Devices[0].Window.height;
     rp_begin.clearValueCount          := 2;
     rp_begin.pClearValues             := @clear_values[0];

     vkCmdBeginRenderPass( _Vulkan.Info.cmd, @rp_begin, VK_SUBPASS_CONTENTS_INLINE );

     vkCmdBindPipeline( _Vulkan.Info.cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, _Pipeline.Handle );
     vkCmdBindDescriptorSets( _Vulkan.Info.cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, _Vulkan.Info.pipeline_layout, 0, NUM_DESCRIPTOR_SETS,
                              @_Vulkan.Info.desc_set[0], 0, nil );

     offsets[0] := 0;
     vkCmdBindVertexBuffers( _Vulkan.Info.cmd, 0, 1, @_Vulkan.Info.vertex_buffer.buf, @offsets[0] );

     init_viewports( _Vulkan );
     init_scissors( _Vulkan );

     vkCmdDraw( _Vulkan.Info.cmd, 12 * 3, 1, 0, 0 );
     vkCmdEndRenderPass( _Vulkan.Info.cmd );
     res := vkEndCommandBuffer( _Vulkan.Info.cmd );
     Assert( res = VK_SUCCESS );

     cmd_bufs[0] := _Vulkan.Info.cmd;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( _Vulkan.Instance.Devices.Devices[0].Handle, @fenceInfo, nil, @drawFence );

     pipe_stage_flags := Ord( VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT );
     submit_info[0]                      := Default( VkSubmitInfo );
     submit_info[0].pNext                := nil;
     submit_info[0].sType                := VK_STRUCTURE_TYPE_SUBMIT_INFO;
     submit_info[0].waitSemaphoreCount   := 1;
     submit_info[0].pWaitSemaphores      := @imageAcquiredSemaphore;
     submit_info[0].pWaitDstStageMask    := @pipe_stage_flags;
     submit_info[0].commandBufferCount   := 1;
     submit_info[0].pCommandBuffers      := @cmd_bufs[0];
     submit_info[0].signalSemaphoreCount := 0;
     submit_info[0].pSignalSemaphores    := nil;

     (* Queue the command buffer for execution *)
     res := vkQueueSubmit( _Vulkan.Info.graphics_queue, 1, @submit_info[0], drawFence );
     Assert( res = VK_SUCCESS );

     (* Now present the image in the window *)

     present.sType              := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
     present.pNext              := nil;
     present.swapchainCount     := 1;
     present.pSwapchains        := @_Vulkan.Info.swap_chain;
     present.pImageIndices      := @_Vulkan.Info.current_buffer;
     present.pWaitSemaphores    := nil;
     present.waitSemaphoreCount := 0;
     present.pResults           := nil;

     (* Make sure command buffer is finished before presenting *)
     repeat
           res := vkWaitForFences( _Vulkan.Instance.Devices.Devices[0].Handle, 1, @drawFence, VK_TRUE, FENCE_TIMEOUT );
     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );
     res := vkQueuePresentKHR( _Vulkan.Info.present_queue, @present );
     Assert( res = VK_SUCCESS );

     wait_seconds( 1 );
     (* VULKAN_KEY_END *)
     if _Vulkan.Info.save_images then write_ppm( _Vulkan, 'draw_textured_cube' );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyFence( _Vulkan.Instance.Devices.Devices[0].Handle, drawFence, nil );
     vkDestroySemaphore( _Vulkan.Instance.Devices.Devices[0].Handle, imageAcquiredSemaphore, nil );
     _Pipeline.Free;
     destroy_pipeline_cache( _Vulkan );
     destroy_textures( _Vulkan );
     destroy_descriptor_pool( _Vulkan );
     destroy_vertex_buffer( _Vulkan );
     destroy_framebuffers( _Vulkan );
     destroy_renderpass( _Vulkan );
     destroy_descriptor_and_pipeline_layouts( _Vulkan );
     destroy_uniform_buffer( _Vulkan );
     destroy_depth_buffer( _Vulkan );
     destroy_swap_chain( _Vulkan );
     destroy_command_buffer( _Vulkan );
     destroy_command_pool( _Vulkan );
     //destroy_window( _Vulkan );
     _Window.Free;
     _Vulkan.Instance.Free;

     _Vulkan.Free;
end;

end. //######################################################################### ■
