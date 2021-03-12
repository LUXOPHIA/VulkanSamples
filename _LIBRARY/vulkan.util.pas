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
       save_images        :T_bool;

       framebuffers  :TArray<VkFramebuffer>;

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

       pipeline_layout :VkPipelineLayout;
       desc_layout     :TArray<VkDescriptorSetLayout>;
       pipelineCache   :VkPipelineCache;
       render_pass     :VkRenderPass;

       desc_pool :VkDescriptorPool;
       desc_set  :TArray<VkDescriptorSet>;

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

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

end. //######################################################################### ■