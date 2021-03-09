unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  WinApi.Windows,
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
    function CreateWindow( const ClientW_,ClientH_:Integer ) :HWND;
    procedure DestroWindow( const Window_:HWND );
  public
    { public 宣言 }
    _Vulkan  :TVulkan;
    _Instan  :TVkInstan;
    _Window  :HWND;
    _Surfac  :TVkSurfac;
    _Device  :TVkDevice;
    _Pooler  :TVkPooler;
    _Comman  :TVkComman;
    _Swapch  :TVkSwapchain;
    _Buffer  :TVkBuffer;
    _Pipeli  :TVkPipeline;
    _ShaderV :TVkShaderVert;
    _ShaderF :TVkShaderFrag;

    imageAcquiredSemaphore :VkSemaphore;
    drawFence              :VkFence;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses WinApi.Messages,
     cube_data;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure run( var info:T_sample_info );
begin
     (* Placeholder for samples that want to show dynamic content *)
end;

function WndProc( hwnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM ) :LRESULT; stdcall;
var
   Info :P_sample_info;
begin
     Info := P_sample_info( GetWindowLongPtr( hWnd, GWLP_USERDATA ) );

     case uMsg of
       WM_CLOSE:
          PostQuitMessage( 0 );
       WM_PAINT:
          begin
               run( Info^ );
               //Exit( 0 );
          end;
     else
     end;

     Result := DefWindowProc( hWnd, uMsg, wParam, lParam );
end;

function TForm1.CreateWindow( const ClientW_,ClientH_:Integer ) :HWND;
var
   C :LPCWSTR;
   M :HINST;
   W :WNDCLASSEX;
   R :TRect;
begin
     C := 'Sample';
     M := HInstance;

     // Initialize the window class structure:
     with W do
     begin
          cbSize        := SizeOf( WNDCLASSEX );
          style         := CS_HREDRAW or CS_VREDRAW;
          lpfnWndProc   := @WndProc;
          cbClsExtra    := 0;
          cbWndExtra    := 0;
          hInstance     := M;  // hInstance
          hIcon         := LoadIcon( 0, IDI_APPLICATION );
          hCursor       := LoadCursor( 0, IDC_ARROW );
          hbrBackground := HBRUSH( GetStockObject( WHITE_BRUSH ) );
          lpszMenuName  := nil;
          lpszClassName := C;
          hIconSm       := LoadIcon( 0, IDI_WINLOGO );
     end;

     // Register window class:
     if RegisterClassEx( W ) = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Unexpected error trying to start the application!' );
          RunError( 1 );
     end;

     // Create window with the registered class:
     R := TRect.Create( 0, 0, ClientW_, ClientH_ );
     AdjustWindowRect( R, WS_OVERLAPPEDWINDOW, False );

     Result := CreateWindowEx( 0,
                               C,                                                // class name
                               C,                                                // app name
                               WS_OVERLAPPEDWINDOW or WS_VISIBLE or WS_SYSMENU,  // window style
                               100, 100,                                         // x/y coords
                               R.Width,                                          // width
                               R.Height,                                         // height
                               0,                                                // handle to parent
                               0,                                                // handle to menu
                               M,                                                // hInstance
                               nil );                                            // no extra parameters

     if Result = 0 then
     begin
          // It didn't work, so try to give a useful error:
          Log.d( 'Cannot create a window in which to draw!' );
          RunError( 1 );
     end;

     SetWindowLongPtr( Result, GWLP_USERDATA, LONG_PTR( @_Vulkan.Info ) );
end;

procedure TForm1.DestroWindow( const Window_:HWND );
begin
     DestroyWindow( Window_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

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
     _Vulkan  := TVulkan.Create;
     _Instan  := TVkInstan.Create( _Vulkan );  //= _Vulkan.Instans.Add;
     _Device  := _Instan.Devices[0];
     _Window  := CreateWindow( 500, 500 );
     _Surfac  := TVkSurfac.Create( _Instan, _Window );
     _Device.Surfac := _Surfac;
     _Pooler  := TVkPooler.Create( _Device );
     _Comman  := TVkComman.Create( _Pooler );  //= _Pooler.Commans.Add;
     _Comman.BeginRecord;
     _Swapch  := TVkSwapchain.Create( _Device );
     init_depth_buffer( _Vulkan );
     init_texture( _Vulkan );
     _Buffer  := TVkBuffer.Create( _Device );
     init_descriptor_and_pipeline_layouts( _Vulkan, true );
     init_renderpass( _Vulkan, depthPresent );
     init_framebuffers( _Vulkan, depthPresent );
     init_vertex_buffer( _Vulkan, @g_vb_texture_Data[0], SizeOf( T_VertexUV ) * Length( g_vb_texture_Data ), SizeOf( T_VertexUV ), True );
     init_descriptor_pool( _Vulkan, True );
     init_descriptor_set( _Vulkan, True );
     init_pipeline_cache( _Vulkan );
     _Pipeli  := TVkPipeline.Create( _Device, depthPresent );
     _ShaderV := TVkShaderVert.Create( _Pipeli );
     _ShaderF := TVkShaderFrag.Create( _Pipeli );
     _ShaderV.LoadFromFile( '../../_DATA/draw_textured_cube.vert' );
     _ShaderF.LoadFromFile( '../../_DATA/draw_textured_cube.frag' );

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

     res := vkCreateSemaphore(  _Device.Handle, @imageAcquiredSemaphoreCreateInfo, nil, @imageAcquiredSemaphore );
     Assert( res = VK_SUCCESS );

     // Get the index of the next available swapchain image:
     res := vkAcquireNextImageKHR( _Device.Handle,  _Swapch.Handle, UINT64_MAX, imageAcquiredSemaphore, VK_NULL_HANDLE,
                                   @_Swapch.Viewers.ViewerI );
     // TODO: Deal with the VK_SUBOPTIMAL_KHR and VK_ERROR_OUT_OF_DATE_KHR
     // return codes
     Assert( res = VK_SUCCESS );

     rp_begin.sType                    := VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
     rp_begin.pNext                    := nil;
     rp_begin.renderPass               :=  _Vulkan.Info.render_pass;
     rp_begin.framebuffer              := _Vulkan.Info.framebuffers[ _Swapch.Viewers.ViewerI ];
     rp_begin.renderArea.offset.x      := 0;
     rp_begin.renderArea.offset.y      := 0;
     rp_begin.renderArea.extent.width  := _Surfac.PxSizeX;
     rp_begin.renderArea.extent.height := _Surfac.PxSizeY;
     rp_begin.clearValueCount          := 2;
     rp_begin.pClearValues             := @clear_values[0];

     vkCmdBeginRenderPass( _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, @rp_begin, VK_SUBPASS_CONTENTS_INLINE );

     vkCmdBindPipeline( _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, VK_PIPELINE_BIND_POINT_GRAPHICS, _Pipeli.Handle );
     vkCmdBindDescriptorSets( _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, VK_PIPELINE_BIND_POINT_GRAPHICS, _Vulkan.Info.pipeline_layout, 0, NUM_DESCRIPTOR_SETS,
                              @_Vulkan.Info.desc_set[0], 0, nil );

     offsets[0] := 0;
     vkCmdBindVertexBuffers( _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, 0, 1, @_Vulkan.Info.vertex_buffer.buf, @offsets[0] );

     init_viewports( _Vulkan );
     init_scissors( _Vulkan );

     vkCmdDraw( _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle, 12 * 3, 1, 0, 0 );
     vkCmdEndRenderPass( _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle );
     _Comman.EndRecord;

     cmd_bufs[0] := _Vulkan.Instans[0].Devices[0].Poolers[0].Commans[0].Handle;
     fenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
     fenceInfo.pNext := nil;
     fenceInfo.flags := 0;
     vkCreateFence( _Device.Handle, @fenceInfo, nil, @drawFence );

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
     res := vkQueueSubmit( _Device.QueuerG, 1, @submit_info[0], drawFence );
     Assert( res = VK_SUCCESS );

     (* Now present the image in the window *)

     present.sType              := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
     present.pNext              := nil;
     present.swapchainCount     := 1;
     present.pSwapchains        := @_Swapch.Handle;
     present.pImageIndices      := @_Swapch.Viewers.ViewerI;
     present.pWaitSemaphores    := nil;
     present.waitSemaphoreCount := 0;
     present.pResults           := nil;

     (* Make sure command buffer is finished before presenting *)
     repeat
           res := vkWaitForFences( _Device.Handle, 1, @drawFence, VK_TRUE, FENCE_TIMEOUT );
     until res <> VK_TIMEOUT;
     Assert( res = VK_SUCCESS );
     res := vkQueuePresentKHR( _Device.QueuerP, @present );
     Assert( res = VK_SUCCESS );

     wait_seconds( 1 );
     (* VULKAN_KEY_END *)
     if _Vulkan.Info.save_images then write_ppm( _Vulkan, 'draw_textured_cube' );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     vkDestroyFence( _Device.Handle, drawFence, nil );
     vkDestroySemaphore( _Device.Handle, imageAcquiredSemaphore, nil );
     _Pipeli.Free;
     destroy_pipeline_cache( _Vulkan );
     destroy_textures( _Vulkan );
     destroy_descriptor_pool( _Vulkan );
     destroy_vertex_buffer( _Vulkan );
     destroy_framebuffers( _Vulkan );
     destroy_renderpass( _Vulkan );
     destroy_descriptor_and_pipeline_layouts( _Vulkan );
     destroy_depth_buffer( _Vulkan );
     _Swapch.Free;
     DestroWindow( _Window );
     _Vulkan.Free;
end;

end. //######################################################################### ■
