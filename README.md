# Vulkan Samples
How to implement [Vulkan](https://www.khronos.org/vulkan/) in [Delphi](https://www.embarcadero.com/products/delphi).  
[Vulkan](https://jp.khronos.org/vulkan/) を [Delphi](https://www.embarcadero.com/jp/products/delphi) で実装する方法。

> ![](https://github.com/LUXOPHIA/VulkanSamples/raw/15-draw_cube/--------/_SCREENSHOT/15-draw_cube.png)  
> Fig.1: 15-draw_cube

## ▼ [VulkanSamples](https://github.com/LunarG/VulkanSamples) @ [LunarG](https://github.com/LunarG)

* [01-init_instance](https://github.com/LUXOPHIA/VulkanSamples/tree/01-init_instance)
  ```delphi
  function init_global_extension_properties( var layer_props_:T_layer_properties ) :VkResult;
  function init_global_layer_properties( var info_:T_sample_info ) :VkResult;
  ```
* [02-enumerate_devices](https://github.com/LUXOPHIA/VulkanSamples/tree/02-enumerate_devices)
  ```delphi
  function init_instance( var info_:T_sample_info; const app_short_name_:P_char ) :VkResult;
  ```
* [03-init_device](https://github.com/LUXOPHIA/VulkanSamples/tree/03-init_device)
  ```delphi
  function init_device_extension_properties( var info_:T_sample_info; var layer_props_:T_layer_properties ) :VkResult;
  function init_enumerate_device( var info_:T_sample_info; gpu_count_:T_uint32_t = 1 ) :VkResult;
  procedure destroy_instance( var info_:T_sample_info );
  ```
* [04-init_command_buffer](https://github.com/LUXOPHIA/VulkanSamples/tree/04-init_command_buffer)
  ```delphi
  procedure init_queue_family_index( var info_:T_sample_info );
  function init_device( var info_:T_sample_info ) :VkResult;
  procedure destroy_device( var info_:T_sample_info );
  ```
* [05-init_swapchain](https://github.com/LUXOPHIA/VulkanSamples/tree/05-init_swapchain)
  ```delphi
  procedure init_instance_extension_names( var info_:T_sample_info );
  procedure init_device_extension_names( var info_:T_sample_info );
  procedure init_window_size( var info_:T_sample_info; default_width_,default_height_:UInt32 );
  procedure init_connection( var info_:T_sample_info );
  procedure init_window( var info_:T_sample_info );
  procedure destroy_window( var info_:T_sample_info );
  ```
* [06-init_depth_buffer](https://github.com/LUXOPHIA/VulkanSamples/tree/06-init_depth_buffer)
  ```delphi
  procedure init_swapchain_extension( var info_:T_sample_info );
  ```
* [07-init_uniform_buffer](https://github.com/LUXOPHIA/VulkanSamples/tree/07-init_uniform_buffer)
* [08-init_pipeline_layout](https://github.com/LUXOPHIA/VulkanSamples/tree/08-init_pipeline_layout)
* [09-init_descriptor_set](https://github.com/LUXOPHIA/VulkanSamples/tree/09-init_descriptor_set)
  ```delphi
  procedure init_uniform_buffer( var info_:T_sample_info );
  procedure init_descriptor_and_pipeline_layouts( var info_:T_sample_info; use_texture_:T_bool; descSetLayoutCreateFlags_:VkDescriptorSetLayoutCreateFlags = 0 );
  procedure destroy_uniform_buffer( var info_:T_sample_info );
  procedure destroy_descriptor_and_pipeline_layouts( var info_:T_sample_info );
  ```
* [10-init_render_pass](https://github.com/LUXOPHIA/VulkanSamples/tree/10-init_render_pass)
  ```delphi
  procedure init_device_queue( var info_:T_sample_info );
  procedure init_swap_chain( var info_:T_sample_info; usageFlags_:VkImageUsageFlags = Ord( VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ) or Ord( VK_IMAGE_USAGE_TRANSFER_SRC_BIT ) );
  procedure init_depth_buffer( var info_:T_sample_info );
  procedure destroy_depth_buffer( var info_:T_sample_info );
  procedure destroy_swap_chain( var info_:T_sample_info );
  ```
* [11-init_shaders](https://github.com/LUXOPHIA/VulkanSamples/tree/11-init_shaders)
* [12-init_frame_buffers](https://github.com/LUXOPHIA/VulkanSamples/tree/12-init_frame_buffers)
  ```delphi
  procedure init_command_pool( var info_:T_sample_info );
  procedure init_command_buffer( var info_:T_sample_info );
  procedure execute_begin_command_buffer( var info_:T_sample_info );
  procedure init_renderpass( var info_:T_sample_info; include_depth_:T_bool; clear_:T_bool = True; finalLayout_:VkImageLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR; initialLayout_:VkImageLayout = VK_IMAGE_LAYOUT_UNDEFINED );
  procedure execute_end_command_buffer( var info_:T_sample_info );
  procedure execute_queue_command_buffer( var info_:T_sample_info );
  procedure destroy_renderpass( var info_:T_sample_info );
  procedure destroy_command_buffer( var info_:T_sample_info );
  procedure destroy_command_pool( var info_:T_sample_info );
  ```
* [13-init_vertex_buffer](https://github.com/LUXOPHIA/VulkanSamples/tree/13-init_vertex_buffer)
  ```delphi
  procedure init_framebuffers( var info_:T_sample_info; include_depth:T_bool );
  procedure destroy_framebuffers( var info_:T_sample_info );
  ```
* [14-init_pipeline](https://github.com/LUXOPHIA/VulkanSamples/tree/14-init_pipeline)
  ```delphi
  procedure init_vertex_buffer( var info_:T_sample_info; const vertexData_:P_void; dataSize_:T_uint32_t; dataStride_:T_uint32_t; use_texture_:T_bool );
  procedure init_descriptor_pool( var info_:T_sample_info; use_texture_:T_bool );
  procedure init_descriptor_set( var info_:T_sample_info; use_texture_:T_bool );
  procedure init_shaders( var info_:T_sample_info; const vertShaderCI_:P_VkShaderModuleCreateInfo; const fragShaderCI_:P_VkShaderModuleCreateInfo );
  procedure destroy_descriptor_pool( var info_:T_sample_info );
  procedure destroy_vertex_buffer( var info_:T_sample_info );
  procedure destroy_shaders( var info_:T_sample_info );
  ```
* [15-draw_cube](https://github.com/LUXOPHIA/VulkanSamples/tree/15-draw_cube)
  ```delphi
  procedure init_viewports( var info_:T_sample_info );
  procedure init_scissors( var info_:T_sample_info );
  procedure init_pipeline_cache( var info:T_sample_info );
  procedure init_pipeline( var info:T_sample_info; include_depth:VkBool32; include_vi:VkBool32 = 1 );
  procedure destroy_pipeline( var info_:T_sample_info );
  procedure destroy_pipeline_cache( var info_:T_sample_info );
  ```
* [16-vulkan_1_1](https://github.com/LUXOPHIA/VulkanSamples/tree/16-vulkan_1_1)
