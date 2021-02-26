# Vulkan Samples
How to implement [Vulkan](https://www.khronos.org/vulkan/) in [Delphi](https://www.embarcadero.com/products/delphi).  
[Vulkan](https://jp.khronos.org/vulkan/) を [Delphi](https://www.embarcadero.com/jp/products/delphi) で実装する方法。

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
