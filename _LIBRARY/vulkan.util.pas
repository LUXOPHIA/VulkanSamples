unit vulkan.util;

(*
 * Vulkan Samples
 *
 * Copyright (C) 2015-2016 Valve Corporation
 * Copyright (C) 2015-2016 LunarG, Inc.
 * Copyright (C) 2015-2016 Google, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

interface //#################################################################### ■

uses vulkan_core,
     LUX.Code.C,
     LUX.D1, LUX.D2, LUX.D2x2, LUX.D3, LUX.D3x3, LUX.D4x4;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_texture_object
     (*
      * structure to track all objects related to a texture.
      *)
     T_texture_object = record
       sampler       :VkSampler;

       image         :VkImage;
       imageLayout   :VkImageLayout;

       needs_staging :T_bool;
       buffer        :VkBuffer;
       buffer_size   :VkDeviceSize;

       image_memory  :VkDeviceMemory;
       buffer_memory :VkDeviceMemory;
       view          :VkImageView;
       tex_width     :T_int32_t;
       tex_height    :T_int32_t;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_swap_chain_buffer
     (*
      * Keep each of our swap chain buffers' image, command buffer and view in one
      * spot
      *)
     T_swap_chain_buffer = record
       image :VkImage;
       view  :VkImageView;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_layer_properties
     (*
      * A layer can expose extensions, keep track of those
      * extensions here.
      *)
     T_layer_properties = record
       properties          :VkLayerProperties;
       instance_extensions :TArray<VkExtensionProperties>;
       device_extensions   :TArray<VkExtensionProperties>;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% T_sample_info
     (*
      * Structure for tracking information used / created / modified
      * by utility functions.
      *)
     P_sample_info = ^T_sample_info;
     T_sample_info = record
     private
       const APP_NAME_STR_LEN = 80;
     public
       prepared           :T_bool;
       use_staging_buffer :T_bool;
       save_images        :T_bool;

       device_extension_properties :TArray<VkExtensionProperties>;
       graphics_queue              :VkQueue;
       present_queue               :VkQueue;
       graphics_queue_family_index :T_uint32_t;
       present_queue_family_index  :T_uint32_t;
       queue_props                 :TArray<VkQueueFamilyProperties>;
       memory_properties           :VkPhysicalDeviceMemoryProperties;

       framebuffers  :TArray<VkFramebuffer>;

       swapchainImageCount    :T_uint32_t;
       swap_chain             :VkSwapchainKHR;
       buffers                :TArray<T_swap_chain_buffer>;
       imageAcquiredSemaphore :VkSemaphore;

       cmd_pool :VkCommandPool;

       depth :record
                format :VkFormat;
                image  :VkImage;
                mem    :VkDeviceMemory;
                view   :VkImageView;
              end;

       textures :TArray<T_texture_object>;

       uniform_data :record
                      buf         :VkBuffer;
                      mem         :VkDeviceMemory;
                      buffer_info :VkDescriptorBufferInfo;
                    end;

       texture_data :record
                       image_info :VkDescriptorImageInfo;
                     end;

       vertex_buffer :record
                        buf         :VkBuffer;
                        mem         :VkDeviceMemory;
                        buffer_info :VkDescriptorBufferInfo;
                      end;

       vi_binding    :VkVertexInputBindingDescription;
       vi_attribs    :array [ 0..2-1 ] of VkVertexInputAttributeDescription;

       Projection :TSingleM4;
       View       :TSingleM4;
       Model      :TSingleM4;
       Clip       :TSingleM4;
       MVP        :TSingleM4;

       cmd             :VkCommandBuffer; // Buffer for initialization commands
       pipeline_layout :VkPipelineLayout;
       desc_layout     :TArray<VkDescriptorSetLayout>;
       pipelineCache   :VkPipelineCache;
       render_pass     :VkRenderPass;

       desc_pool :VkDescriptorPool;
       desc_set  :TArray<VkDescriptorSet>;

       dbgCreateDebugReportCallback  :PFN_vkCreateDebugReportCallbackEXT;
       dbgDestroyDebugReportCallback :PFN_vkDestroyDebugReportCallbackEXT;
       dbgBreakCallback              :PFN_vkDebugReportMessageEXT;
       debug_report_callbacks        :TArray<VkDebugReportCallbackEXT>;

       current_buffer     :T_uint32_t;

       viewport :VkViewport;
       scissor  :VkRect2D;
     end;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

     (* Number of samples needs to be the same at image creation,      *)
     (* renderpass creation and pipeline creation.                     *)
     NUM_SAMPLES = VK_SAMPLE_COUNT_1_BIT;

     (* Number of descriptor sets needs to be the same at alloc,       *)
     (* pipeline layout creation, and descriptor set layout creation   *)
     NUM_DESCRIPTOR_SETS = 1;

     (* Amount of time, in nanoseconds, to wait for a command buffer to complete *)
     FENCE_TIMEOUT = 100000000;

     (* Number of viewports and number of scissors have to be the same *)
     (* at pipeline creation and in any call to set them dynamically   *)
     (* They also have to be the same as each other                    *)
     NUM_VIEWPORTS = 1;
     NUM_SCISSORS  = NUM_VIEWPORTS;

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.Classes, 
     FMX.Types;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

end. //######################################################################### ■