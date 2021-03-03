unit vulkan_core;

(*
** Copyright 2015-2021 The Khronos Group Inc.
**
** SPDX-License-Identifier: Apache-2.0
*)

(*
** This header is generated from the Khronos Vulkan XML API Registry.
**
*)

interface //#################################################################### ■

uses LUX.Code.C;

const DLLNAME = 'vulkan-1.dll';

const VK_VERSION_1_0 = 1;
////#include "vk_platform.h"

type VK_DEFINE_HANDLE_T = record end;
     VK_DEFINE_HANDLE   = ^VK_DEFINE_HANDLE_T;

{$IF not Defined( VK_DEFINE_NON_DISPATCHABLE_HANDLE ) }
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or ( Defined( __x86_64__ ) and not Defined( __ILP32__ ) ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
        type VK_DEFINE_NON_DISPATCHABLE_HANDLE_T = record end;
             VK_DEFINE_NON_DISPATCHABLE_HANDLE   = ^VK_DEFINE_NON_DISPATCHABLE_HANDLE_T;
{$ELSE}
        type VK_DEFINE_NON_DISPATCHABLE_HANDLE = T_uint64_t;
{$ENDIF}
{$ENDIF}

function VK_MAKE_VERSION( const major_,minor_,patch_:T_uint32_t ) :T_uint32_t; inline;

// DEPRECATED: This define has been removed. Specific version defines (e.g. VK_API_VERSION_1_0), or the VK_MAKE_VERSION macro, should be used instead.
//#define VK_API_VERSION VK_MAKE_VERSION(1, 0, 0) // Patch version should always be set to 0

// Vulkan 1.0 version number
const VK_API_VERSION_1_0 :T_uint32_t = {VK_MAKE_VERSION( 1, 0, 0 )} ( 1 shl 22 ) or ( 2 shl 12 ) or 0; // Patch version should always be set to 0

// Version of this file
const VK_HEADER_VERSION = 170;

// Complete version of this file
const VK_HEADER_VERSION_COMPLETE :T_uint32_t = {VK_MAKE_VERSION( 1, 2, VK_HEADER_VERSION )} ( 1 shl 22 ) or ( 2 shl 12 ) or VK_HEADER_VERSION;

function VK_VERSION_MAJOR( const version_:T_uint32_t ) :T_uint32_t; inline;
function VK_VERSION_MINOR( const version_:T_uint32_t ) :T_uint32_t; inline;
function VK_VERSION_PATCH( const version_:T_uint32_t ) :T_uint32_t; inline;

const VK_NULL_HANDLE = 0;

type VkBool32              = T_uint32_t;                         P_VkBool32              = ^VkBool32;
type VkDeviceAddress       = T_uint64_t;                         P_VkDeviceAddress       = ^VkDeviceAddress;
type VkDeviceSize          = T_uint64_t;                         P_VkDeviceSize          = ^VkDeviceSize;
type VkFlags               = T_uint32_t;                         P_VkFlags               = ^VkFlags;
type VkSampleMask          = T_uint32_t;                         P_VkSampleMask          = ^VkSampleMask;
type VkBuffer              = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkBuffer              = ^VkBuffer;
type VkImage               = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkImage               = ^VkImage;
type VkInstance            = VK_DEFINE_HANDLE;                   P_VkInstance            = ^VkInstance;
type VkPhysicalDevice      = VK_DEFINE_HANDLE;                   P_VkPhysicalDevice      = ^VkPhysicalDevice;
type VkDevice              = VK_DEFINE_HANDLE;                   P_VkDevice              = ^VkDevice;
type VkQueue               = VK_DEFINE_HANDLE;                   P_VkQueue               = ^VkQueue;
type VkSemaphore           = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkSemaphore           = ^VkSemaphore;
type VkCommandBuffer       = VK_DEFINE_HANDLE;                   P_VkCommandBuffer       = ^VkCommandBuffer;
type VkFence               = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkFence               = ^VkFence;
type VkDeviceMemory        = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkDeviceMemory        = ^VkDeviceMemory;
type VkEvent               = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkEvent               = ^VkEvent;
type VkQueryPool           = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkQueryPool           = ^VkQueryPool;
type VkBufferView          = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkBufferView          = ^VkBufferView;
type VkImageView           = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkImageView           = ^VkImageView;
type VkShaderModule        = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkShaderModule        = ^VkShaderModule;
type VkPipelineCache       = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkPipelineCache       = ^VkPipelineCache;
type VkPipelineLayout      = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkPipelineLayout      = ^VkPipelineLayout;
type VkPipeline            = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkPipeline            = ^VkPipeline;
type VkRenderPass          = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkRenderPass          = ^VkRenderPass;
type VkDescriptorSetLayout = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkDescriptorSetLayout = ^VkDescriptorSetLayout;
type VkSampler             = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkSampler             = ^VkSampler;
type VkDescriptorSet       = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkDescriptorSet       = ^VkDescriptorSet;
type VkDescriptorPool      = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkDescriptorPool      = ^VkDescriptorPool;
type VkFramebuffer         = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkFramebuffer         = ^VkFramebuffer;
type VkCommandPool         = VK_DEFINE_NON_DISPATCHABLE_HANDLE;  P_VkCommandPool         = ^VkCommandPool;
const VK_ATTACHMENT_UNUSED              = UInt32( $FFFFFFFF ); {(~0U)}
const VK_FALSE                          = 0;
const VK_LOD_CLAMP_NONE                 = T_float( 1000.0 );
const VK_QUEUE_FAMILY_IGNORED           = UInt32( $FFFFFFFF ); {(~0U)}
const VK_REMAINING_ARRAY_LAYERS         = UInt32( $FFFFFFFF ); {(~0U)}
const VK_REMAINING_MIP_LEVELS           = UInt32( $FFFFFFFF ); {(~0U)}
const VK_SUBPASS_EXTERNAL               = UInt32( $FFFFFFFF ); {(~0U)}
const VK_TRUE                           = 1;
const VK_WHOLE_SIZE                     = UInt64( $FFFFFFFFFFFFFFFF ); {(~0ULL)}
const VK_MAX_MEMORY_TYPES               = 32;
const VK_MAX_MEMORY_HEAPS               = 16;
const VK_MAX_PHYSICAL_DEVICE_NAME_SIZE  = 256;
const VK_UUID_SIZE                      = 16;
const VK_MAX_EXTENSION_NAME_SIZE        = 256;
const VK_MAX_DESCRIPTION_SIZE           = 256;

type P_VkResult = ^VkResult;
     VkResult = (
       VK_SUCCESS                                            = 0,
       VK_NOT_READY                                          = 1,
       VK_TIMEOUT                                            = 2,
       VK_EVENT_SET                                          = 3,
       VK_EVENT_RESET                                        = 4,
       VK_INCOMPLETE                                         = 5,
       VK_ERROR_OUT_OF_HOST_MEMORY                           = -1,
       VK_ERROR_OUT_OF_DEVICE_MEMORY                         = -2,
       VK_ERROR_INITIALIZATION_FAILED                        = -3,
       VK_ERROR_DEVICE_LOST                                  = -4,
       VK_ERROR_MEMORY_MAP_FAILED                            = -5,
       VK_ERROR_LAYER_NOT_PRESENT                            = -6,
       VK_ERROR_EXTENSION_NOT_PRESENT                        = -7,
       VK_ERROR_FEATURE_NOT_PRESENT                          = -8,
       VK_ERROR_INCOMPATIBLE_DRIVER                          = -9,
       VK_ERROR_TOO_MANY_OBJECTS                             = -10,
       VK_ERROR_FORMAT_NOT_SUPPORTED                         = -11,
       VK_ERROR_FRAGMENTED_POOL                              = -12,
       VK_ERROR_UNKNOWN                                      = -13,
       VK_ERROR_OUT_OF_POOL_MEMORY                           = -1000069000,
       VK_ERROR_INVALID_EXTERNAL_HANDLE                      = -1000072003,
       VK_ERROR_FRAGMENTATION                                = -1000161000,
       VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS               = -1000257000,
       VK_ERROR_SURFACE_LOST_KHR                             = -1000000000,
       VK_ERROR_NATIVE_WINDOW_IN_USE_KHR                     = -1000000001,
       VK_SUBOPTIMAL_KHR                                     = 1000001003,
       VK_ERROR_OUT_OF_DATE_KHR                              = -1000001004,
       VK_ERROR_INCOMPATIBLE_DISPLAY_KHR                     = -1000003001,
       VK_ERROR_VALIDATION_FAILED_EXT                        = -1000011001,
       VK_ERROR_INVALID_SHADER_NV                            = -1000012000,
       VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT = -1000158000,
       VK_ERROR_NOT_PERMITTED_EXT                            = -1000174001,
       VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT          = -1000255000,
       VK_THREAD_IDLE_KHR                                    = 1000268000,
       VK_THREAD_DONE_KHR                                    = 1000268001,
       VK_OPERATION_DEFERRED_KHR                             = 1000268002,
       VK_OPERATION_NOT_DEFERRED_KHR                         = 1000268003,
       VK_PIPELINE_COMPILE_REQUIRED_EXT                      = 1000297000,
       VK_ERROR_OUT_OF_POOL_MEMORY_KHR                       = VK_ERROR_OUT_OF_POOL_MEMORY,
       VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR                  = VK_ERROR_INVALID_EXTERNAL_HANDLE,
       VK_ERROR_FRAGMENTATION_EXT                            = VK_ERROR_FRAGMENTATION,
       VK_ERROR_INVALID_DEVICE_ADDRESS_EXT                   = VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS,
       VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR           = VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS,
       VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT                = VK_PIPELINE_COMPILE_REQUIRED_EXT,
       VK_RESULT_MAX_ENUM                                    = $7FFFFFFF
     );

type P_VkStructureType = ^VkStructureType;
     VkStructureType = (
       VK_STRUCTURE_TYPE_APPLICATION_INFO                                                = 0,
       VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO                                            = 1,
       VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO                                        = 2,
       VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO                                              = 3,
       VK_STRUCTURE_TYPE_SUBMIT_INFO                                                     = 4,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO                                            = 5,
       VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE                                             = 6,
       VK_STRUCTURE_TYPE_BIND_SPARSE_INFO                                                = 7,
       VK_STRUCTURE_TYPE_FENCE_CREATE_INFO                                               = 8,
       VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO                                           = 9,
       VK_STRUCTURE_TYPE_EVENT_CREATE_INFO                                               = 10,
       VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO                                          = 11,
       VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO                                              = 12,
       VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO                                         = 13,
       VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO                                               = 14,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO                                          = 15,
       VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO                                       = 16,
       VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO                                      = 17,
       VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO                               = 18,
       VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO                         = 19,
       VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO                       = 20,
       VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO                         = 21,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO                             = 22,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO                        = 23,
       VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO                          = 24,
       VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO                        = 25,
       VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO                          = 26,
       VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO                              = 27,
       VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO                                   = 28,
       VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO                                    = 29,
       VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO                                     = 30,
       VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO                                             = 31,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO                               = 32,
       VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO                                     = 33,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO                                    = 34,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET                                            = 35,
       VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET                                             = 36,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO                                         = 37,
       VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO                                         = 38,
       VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO                                        = 39,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO                                    = 40,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO                                 = 41,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO                                       = 42,
       VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO                                          = 43,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER                                           = 44,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER                                            = 45,
       VK_STRUCTURE_TYPE_MEMORY_BARRIER                                                  = 46,
       VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO                                     = 47,
       VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO                                       = 48,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES                             = 1000094000,
       VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO                                         = 1000157000,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO                                          = 1000157001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES                          = 1000083000,
       VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS                                   = 1000127000,
       VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO                                  = 1000127001,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO                                      = 1000060000,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO                             = 1000060003,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO                          = 1000060004,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO                                        = 1000060005,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO                                   = 1000060006,
       VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO                            = 1000060013,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO                             = 1000060014,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES                                = 1000070000,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO                                 = 1000070001,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2                               = 1000146000,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2                                = 1000146001,
       VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2                         = 1000146002,
       VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2                                           = 1000146003,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2                              = 1000146004,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2                                      = 1000059000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2                                    = 1000059001,
       VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2                                             = 1000059002,
       VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2                                       = 1000059003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2                             = 1000059004,
       VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2                                       = 1000059005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2                             = 1000059006,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2                                = 1000059007,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2                      = 1000059008,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES                       = 1000117000,
       VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO                 = 1000117001,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO                                    = 1000117002,
       VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO           = 1000117003,
       VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO                               = 1000053000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES                              = 1000053001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES                            = 1000053002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES                      = 1000120000,
       VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO                                           = 1000145000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES                       = 1000145001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES                     = 1000145002,
       VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2                                             = 1000145003,
       VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO                            = 1000156000,
       VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO                                   = 1000156001,
       VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO                                    = 1000156002,
       VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO                            = 1000156003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES               = 1000156004,
       VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES                = 1000156005,
       VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO                          = 1000085000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO                      = 1000071000,
       VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES                                = 1000071001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO                            = 1000071002,
       VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES                                      = 1000071003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES                                   = 1000071004,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO                              = 1000072000,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO                               = 1000072001,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO                                     = 1000072002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO                             = 1000112000,
       VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES                                       = 1000112001,
       VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO                                        = 1000113000,
       VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO                                    = 1000077000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO                         = 1000076000,
       VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES                                   = 1000076001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES                        = 1000168000,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT                                   = 1000168001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES                 = 1000063000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES                             = 49,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES                           = 50,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES                             = 51,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES                           = 52,
       VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO                                   = 1000147000,
       VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2                                        = 1000109000,
       VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2                                          = 1000109001,
       VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2                                           = 1000109002,
       VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2                                            = 1000109003,
       VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2                                       = 1000109004,
       VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO                                              = 1000109005,
       VK_STRUCTURE_TYPE_SUBPASS_END_INFO                                                = 1000109006,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES                           = 1000177000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES                               = 1000196000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES                    = 1000180000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES                    = 1000082000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES                       = 1000197000,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO                 = 1000161000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES                    = 1000161001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES                  = 1000161002,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO          = 1000161003,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT         = 1000161004,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES                = 1000199000,
       VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE                       = 1000199001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES                    = 1000221000,
       VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO                                 = 1000246000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES                = 1000130000,
       VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO                              = 1000130001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES                    = 1000211000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES                  = 1000108000,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO                             = 1000108001,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO                               = 1000108002,
       VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO                               = 1000108003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES         = 1000253000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES         = 1000175000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES         = 1000241000,
       VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT                             = 1000241001,
       VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT                           = 1000241002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES                       = 1000261000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES                     = 1000207000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES                   = 1000207001,
       VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO                                      = 1000207002,
       VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO                                  = 1000207003,
       VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO                                             = 1000207004,
       VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO                                           = 1000207005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES                  = 1000257000,
       VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO                                      = 1000244001,
       VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO                       = 1000257002,
       VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO                     = 1000257003,
       VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO                       = 1000257004,
       VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR                                       = 1000001000,
       VK_STRUCTURE_TYPE_PRESENT_INFO_KHR                                                = 1000001001,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR                           = 1000060007,
       VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR                                 = 1000060008,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR                            = 1000060009,
       VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR                                     = 1000060010,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR                                   = 1000060011,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR                          = 1000060012,
       VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR                                    = 1000002000,
       VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR                                 = 1000002001,
       VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR                                        = 1000003000,
       VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR                                    = 1000004000,
       VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR                                     = 1000005000,
       VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR                                 = 1000006000,
       VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR                                 = 1000008000,
       VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR                                   = 1000009000,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT                           = 1000011000,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD            = 1000018000,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT                               = 1000022000,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT                                = 1000022001,
       VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT                                    = 1000022002,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV                       = 1000026000,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV                      = 1000026001,
       VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV                    = 1000026002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT                 = 1000028000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT               = 1000028001,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT             = 1000028002,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX                                      = 1000030000,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX                               = 1000030001,
       VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD                        = 1000041000,
       VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP                       = 1000049000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV                = 1000050000,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV                            = 1000056000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV                                  = 1000056001,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV                              = 1000057000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV                              = 1000057001,
       VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV                       = 1000058000,
       VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT                                            = 1000061000,
       VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN                                       = 1000062000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT       = 1000066000,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT                                 = 1000067000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT                        = 1000067001,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR                             = 1000073000,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR                             = 1000073001,
       VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR                              = 1000073002,
       VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR                                = 1000073003,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR                                       = 1000074000,
       VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR                                        = 1000074001,
       VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR                                          = 1000074002,
       VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR                      = 1000075000,
       VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR                          = 1000078000,
       VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR                          = 1000078001,
       VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR                                     = 1000078002,
       VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR                             = 1000078003,
       VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR                                    = 1000079000,
       VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR                                       = 1000079001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR                  = 1000080000,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT       = 1000081000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT              = 1000081001,
       VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT                            = 1000081002,
       VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR                                             = 1000084000,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV                = 1000087000,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT                                      = 1000090000,
       VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT                                          = 1000091000,
       VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT                                           = 1000091001,
       VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT                                          = 1000091002,
       VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT                               = 1000091003,
       VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE                                       = 1000092000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX    = 1000097000,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV                  = 1000098000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT                = 1000099000,
       VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT                = 1000099001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT       = 1000101000,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT       = 1000101001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT                  = 1000102000,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT         = 1000102001,
       VK_STRUCTURE_TYPE_HDR_METADATA_EXT                                                = 1000105000,
       VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR                         = 1000111000,
       VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR                              = 1000114000,
       VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR                              = 1000114001,
       VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR                                 = 1000114002,
       VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR                                        = 1000115000,
       VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR                                           = 1000115001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR                  = 1000116000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR                = 1000116001,
       VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR                          = 1000116002,
       VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR                               = 1000116003,
       VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR                                 = 1000116004,
       VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR                                         = 1000116005,
       VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR                             = 1000116006,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR                              = 1000119000,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR                                      = 1000119001,
       VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR                                            = 1000119002,
       VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR                                        = 1000121000,
       VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR                                  = 1000121001,
       VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR                                   = 1000121002,
       VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR                                        = 1000121003,
       VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR                                = 1000121004,
       VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK                                     = 1000122000,
       VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK                                   = 1000123000,
       VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT                                = 1000128000,
       VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT                                 = 1000128001,
       VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT                                           = 1000128002,
       VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT                         = 1000128003,
       VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT                           = 1000128004,
       VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID                           = 1000129000,
       VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID                      = 1000129001,
       VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID               = 1000129002,
       VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID                     = 1000129003,
       VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID                 = 1000129004,
       VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID                                         = 1000129005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT               = 1000138000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT             = 1000138001,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT                   = 1000138002,
       VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT            = 1000138003,
       VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT                                       = 1000143000,
       VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT                     = 1000143001,
       VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT                 = 1000143002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT                 = 1000143003,
       VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT                                      = 1000143004,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT           = 1000148000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT         = 1000148001,
       VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT             = 1000148002,
       VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV                 = 1000149000,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR                 = 1000150007,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR                  = 1000150000,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR                  = 1000150002,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR                  = 1000150003,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR              = 1000150004,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR              = 1000150005,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR                             = 1000150006,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_INFO_KHR                         = 1000150009,
       VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR                            = 1000150010,
       VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR                  = 1000150011,
       VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR                  = 1000150012,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR             = 1000150013,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR           = 1000150014,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR                          = 1000150017,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR                     = 1000150020,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR               = 1000347000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR             = 1000347001,
       VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR                            = 1000150015,
       VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR                        = 1000150016,
       VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR                  = 1000150018,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR                          = 1000348013,
       VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV               = 1000152000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV                  = 1000154000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV                = 1000154001,
       VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT                         = 1000158000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT              = 1000158002,
       VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT                  = 1000158003,
       VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT              = 1000158004,
       VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT                        = 1000158005,
       VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT                                = 1000160000,
       VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT                  = 1000160001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR                 = 1000163000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR               = 1000163001,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV       = 1000164000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV                  = 1000164001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV                = 1000164002,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV      = 1000164005,
       VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV                             = 1000165000,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV                           = 1000165001,
       VK_STRUCTURE_TYPE_GEOMETRY_NV                                                     = 1000165003,
       VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV                                           = 1000165004,
       VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV                                                = 1000165005,
       VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV                      = 1000165006,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV                  = 1000165007,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV              = 1000165008,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV                       = 1000165009,
       VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV                         = 1000165011,
       VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV                                  = 1000165012,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV        = 1000166000,
       VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV      = 1000166001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT                = 1000170000,
       VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT             = 1000170001,
       VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT                    = 1000174000,
       VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT                             = 1000178000,
       VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT                              = 1000178001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT             = 1000178002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR                       = 1000181000,
       VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD                       = 1000183000,
       VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT                                   = 1000184000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD                      = 1000185000,
       VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD                    = 1000189000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT         = 1000190000,
       VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT             = 1000190001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT           = 1000190002,
       VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP                                         = 1000191000,
       VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT                      = 1000192000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV          = 1000201000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV                         = 1000202000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV                       = 1000202001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV         = 1000203000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV              = 1000204000,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV        = 1000205000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV                   = 1000205002,
       VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV                                              = 1000206000,
       VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV                           = 1000206001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL       = 1000209000,
       VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL                  = 1000210000,
       VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL                           = 1000210001,
       VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL                                   = 1000210002,
       VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL                            = 1000210003,
       VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL                                 = 1000210004,
       VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL                    = 1000210005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT                     = 1000212000,
       VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD                     = 1000213000,
       VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD                    = 1000213001,
       VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA                           = 1000214000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR        = 1000215000,
       VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT                                   = 1000217000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT               = 1000218000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT             = 1000218001,
       VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT                = 1000218002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT            = 1000225000,
       VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT    = 1000225001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT              = 1000225002,
       VK_STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR                       = 1000226000,
       VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR            = 1000226001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR            = 1000226002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR              = 1000226003,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR                       = 1000226004,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD                    = 1000227000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD                    = 1000229000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT          = 1000234000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT                    = 1000237000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT                    = 1000238000,
       VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT                               = 1000238001,
       VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR                              = 1000239000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV = 1000240000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT              = 1000244000,
       VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT                           = 1000244002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT                             = 1000245000,
       VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT                                         = 1000247000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV                  = 1000249000,
       VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV                                = 1000249001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV                = 1000249002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV             = 1000250000,
       VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV                = 1000250001,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV                        = 1000250002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT          = 1000251000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT                 = 1000252000,
       VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT                          = 1000255000,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT                  = 1000255002,
       VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT                    = 1000255001,
       VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT                                = 1000256000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT                 = 1000259000,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT               = 1000259001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT               = 1000259002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT                = 1000260000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT                   = 1000265000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT             = 1000267000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR     = 1000269000,
       VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR                                               = 1000269001,
       VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR                              = 1000269002,
       VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR                                    = 1000269003,
       VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR                               = 1000269004,
       VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR                 = 1000269005,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT = 1000276000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV         = 1000277000,
       VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV                            = 1000277001,
       VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV                  = 1000277002,
       VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV                               = 1000277003,
       VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV                         = 1000277004,
       VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV                                      = 1000277005,
       VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV                  = 1000277006,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV           = 1000277007,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT             = 1000281000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT           = 1000281001,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM      = 1000282000,
       VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM                           = 1000282001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT               = 1000284000,
       VK_STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT                     = 1000284001,
       VK_STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT                          = 1000284002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT                       = 1000286000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT                     = 1000286001,
       VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT                     = 1000287000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT              = 1000287001,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT                = 1000287002,
       VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR                                = 1000290000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT                       = 1000295000,
       VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT                             = 1000295001,
       VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT                               = 1000295002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT    = 1000297000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV                  = 1000300000,
       VK_STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV                        = 1000300001,
       VK_STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR                                            = 1000314000,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR                                     = 1000314001,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR                                      = 1000314002,
       VK_STRUCTURE_TYPE_DEPENDENCY_INFO_KHR                                             = 1000314003,
       VK_STRUCTURE_TYPE_SUBMIT_INFO_2_KHR                                               = 1000314004,
       VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR                                       = 1000314005,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR                                  = 1000314006,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR                  = 1000314007,
       VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV                         = 1000314008,
       VK_STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV                                            = 1000314009,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR   = 1000325000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV       = 1000326000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV         = 1000326001,
       VK_STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV        = 1000326002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT             = 1000332000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT           = 1000332001,
       VK_STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM                                = 1000333000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT                   = 1000335000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR   = 1000336000,
       VK_STRUCTURE_TYPE_COPY_BUFFER_INFO_2_KHR                                          = 1000337000,
       VK_STRUCTURE_TYPE_COPY_IMAGE_INFO_2_KHR                                           = 1000337001,
       VK_STRUCTURE_TYPE_COPY_BUFFER_TO_IMAGE_INFO_2_KHR                                 = 1000337002,
       VK_STRUCTURE_TYPE_COPY_IMAGE_TO_BUFFER_INFO_2_KHR                                 = 1000337003,
       VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2_KHR                                           = 1000337004,
       VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2_KHR                                        = 1000337005,
       VK_STRUCTURE_TYPE_BUFFER_COPY_2_KHR                                               = 1000337006,
       VK_STRUCTURE_TYPE_IMAGE_COPY_2_KHR                                                = 1000337007,
       VK_STRUCTURE_TYPE_IMAGE_BLIT_2_KHR                                                = 1000337008,
       VK_STRUCTURE_TYPE_BUFFER_IMAGE_COPY_2_KHR                                         = 1000337009,
       VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2_KHR                                             = 1000337010,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT                       = 1000340000,
       VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT                                = 1000346000,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE          = 1000351000,
       VK_STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE                       = 1000351002,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES                       = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES                  = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT                                    = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
       VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR                           = VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR                          = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR                        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR                                  = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR                                = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
       VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR                                         = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
       VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR                                   = VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR                         = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
       VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR                                   = VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR                         = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR                            = VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR                  = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR                                  = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR                         = VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR                      = VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR                                    = VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR                               = VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
       VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR                        = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR                         = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR                            = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES,
       VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR                             = VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR                  = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
       VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR                            = VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR                        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
       VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR                                  = VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR                               = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR                          = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO,
       VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR                           = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
       VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR                                 = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR                     = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO,
       VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR                               = VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
       VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR                                = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES_KHR                = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR                       = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR                      = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES,
       VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR                      = VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
       VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT                                       = VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR              = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR                         = VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR                           = VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO,
       VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR                           = VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO,
       VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR                                    = VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2,
       VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR                                      = VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2,
       VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR                                       = VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2,
       VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR                                        = VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2,
       VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR                                   = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2,
       VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR                                          = VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO,
       VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR                                            = VK_STRUCTURE_TYPE_SUBPASS_END_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR                         = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO,
       VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR                                   = VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES,
       VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR                                    = VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR                   = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES,
       VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR             = VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR                                = VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO,
       VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR       = VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR                  = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR                   = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR,
       VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR                               = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS,
       VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR                              = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT            = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES,
       VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT                          = VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR                           = VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR                            = VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
       VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR                     = VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
       VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR                                       = VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
       VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR                          = VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2,
       VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR                               = VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
       VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR                        = VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO,
       VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR                               = VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO,
       VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR                                = VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO,
       VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR                        = VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR           = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES,
       VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR            = VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES,
       VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR                                     = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO,
       VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR                                      = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT             = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT                = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT              = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT      = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT     = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR                    = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR                               = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR     = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR                       = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR                = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR                           = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR                   = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR            = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES,
       VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR                   = VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR                 = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR               = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES,
       VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR                                  = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
       VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR                              = VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO,
       VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR                                         = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
       VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR                                       = VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO,
       VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL                                    = VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR                = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT                = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR     = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES,
       VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR                         = VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT,
       VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR                       = VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT                     = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT,
       VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT                                  = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
       VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT                             = VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR     = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR              = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES,
       VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR                                  = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
       VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR                   = VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO,
       VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR                 = VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO,
       VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR                   = VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO,
       VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT                   = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES,
       VK_STRUCTURE_TYPE_MAX_ENUM                                                        = $7FFFFFFF
     );

type P_VkImageLayout = ^VkImageLayout;
     VkImageLayout = (
       VK_IMAGE_LAYOUT_UNDEFINED                                      = 0,
       VK_IMAGE_LAYOUT_GENERAL                                        = 1,
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL                       = 2,
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL               = 3,
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL                = 4,
       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL                       = 5,
       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL                           = 6,
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL                           = 7,
       VK_IMAGE_LAYOUT_PREINITIALIZED                                 = 8,
       VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL     = 1000117000,
       VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL     = 1000117001,
       VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL                       = 1000241000,
       VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL                        = 1000241001,
       VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL                     = 1000241002,
       VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL                      = 1000241003,
       VK_IMAGE_LAYOUT_PRESENT_SRC_KHR                                = 1000001002,
       VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR                             = 1000111000,
       VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV                        = 1000164003,
       VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT               = 1000218000,
       VK_IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR                          = 1000314000,
       VK_IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR                         = 1000314001,
       VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR = VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
       VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR = VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,
       VK_IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR   = VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV,
       VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR                   = VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL,
       VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR                    = VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL,
       VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR                 = VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL,
       VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR                  = VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL,
       VK_IMAGE_LAYOUT_MAX_ENUM                                       = $7FFFFFFF
     );

type P_VkObjectType = ^VkObjectType;
     VkObjectType = (
       VK_OBJECT_TYPE_UNKNOWN                         = 0,
       VK_OBJECT_TYPE_INSTANCE                        = 1,
       VK_OBJECT_TYPE_PHYSICAL_DEVICE                 = 2,
       VK_OBJECT_TYPE_DEVICE                          = 3,
       VK_OBJECT_TYPE_QUEUE                           = 4,
       VK_OBJECT_TYPE_SEMAPHORE                       = 5,
       VK_OBJECT_TYPE_COMMAND_BUFFER                  = 6,
       VK_OBJECT_TYPE_FENCE                           = 7,
       VK_OBJECT_TYPE_DEVICE_MEMORY                   = 8,
       VK_OBJECT_TYPE_BUFFER                          = 9,
       VK_OBJECT_TYPE_IMAGE                           = 10,
       VK_OBJECT_TYPE_EVENT                           = 11,
       VK_OBJECT_TYPE_QUERY_POOL                      = 12,
       VK_OBJECT_TYPE_BUFFER_VIEW                     = 13,
       VK_OBJECT_TYPE_IMAGE_VIEW                      = 14,
       VK_OBJECT_TYPE_SHADER_MODULE                   = 15,
       VK_OBJECT_TYPE_PIPELINE_CACHE                  = 16,
       VK_OBJECT_TYPE_PIPELINE_LAYOUT                 = 17,
       VK_OBJECT_TYPE_RENDER_PASS                     = 18,
       VK_OBJECT_TYPE_PIPELINE                        = 19,
       VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT           = 20,
       VK_OBJECT_TYPE_SAMPLER                         = 21,
       VK_OBJECT_TYPE_DESCRIPTOR_POOL                 = 22,
       VK_OBJECT_TYPE_DESCRIPTOR_SET                  = 23,
       VK_OBJECT_TYPE_FRAMEBUFFER                     = 24,
       VK_OBJECT_TYPE_COMMAND_POOL                    = 25,
       VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION        = 1000156000,
       VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE      = 1000085000,
       VK_OBJECT_TYPE_SURFACE_KHR                     = 1000000000,
       VK_OBJECT_TYPE_SWAPCHAIN_KHR                   = 1000001000,
       VK_OBJECT_TYPE_DISPLAY_KHR                     = 1000002000,
       VK_OBJECT_TYPE_DISPLAY_MODE_KHR                = 1000002001,
       VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT       = 1000011000,
       VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT       = 1000128000,
       VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR      = 1000150000,
       VK_OBJECT_TYPE_VALIDATION_CACHE_EXT            = 1000160000,
       VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV       = 1000165000,
       VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL = 1000210000,
       VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR          = 1000268000,
       VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV     = 1000277000,
       VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT           = 1000295000,
       VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR  = VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
       VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR    = VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION,
       VK_OBJECT_TYPE_MAX_ENUM                        = $7FFFFFFF
     );

type P_VkVendorId = ^VkVendorId;
     VkVendorId = (
       VK_VENDOR_ID_VIV      = $10001,
       VK_VENDOR_ID_VSI      = $10002,
       VK_VENDOR_ID_KAZAN    = $10003,
       VK_VENDOR_ID_CODEPLAY = $10004,
       VK_VENDOR_ID_MESA     = $10005,
       VK_VENDOR_ID_POCL     = $10006,
       VK_VENDOR_ID_MAX_ENUM = $7FFFFFFF
     );

type P_VkPipelineCacheHeaderVersion = ^VkPipelineCacheHeaderVersion;
     VkPipelineCacheHeaderVersion = (
       VK_PIPELINE_CACHE_HEADER_VERSION_ONE      = 1,
       VK_PIPELINE_CACHE_HEADER_VERSION_MAX_ENUM = $7FFFFFFF
     );

type P_VkSystemAllocationScope = ^VkSystemAllocationScope;
     VkSystemAllocationScope = (
       VK_SYSTEM_ALLOCATION_SCOPE_COMMAND  = 0,
       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT   = 1,
       VK_SYSTEM_ALLOCATION_SCOPE_CACHE    = 2,
       VK_SYSTEM_ALLOCATION_SCOPE_DEVICE   = 3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = 4,
       VK_SYSTEM_ALLOCATION_SCOPE_MAX_ENUM = $7FFFFFFF
     );

type P_VkInternalAllocationType = ^VkInternalAllocationType;
     VkInternalAllocationType = (
       VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = 0,
       VK_INTERNAL_ALLOCATION_TYPE_MAX_ENUM   = $7FFFFFFF
     );

type P_VkFormat = ^VkFormat;
     VkFormat = (
       VK_FORMAT_UNDEFINED                                      = 0,
       VK_FORMAT_R4G4_UNORM_PACK8                               = 1,
       VK_FORMAT_R4G4B4A4_UNORM_PACK16                          = 2,
       VK_FORMAT_B4G4R4A4_UNORM_PACK16                          = 3,
       VK_FORMAT_R5G6B5_UNORM_PACK16                            = 4,
       VK_FORMAT_B5G6R5_UNORM_PACK16                            = 5,
       VK_FORMAT_R5G5B5A1_UNORM_PACK16                          = 6,
       VK_FORMAT_B5G5R5A1_UNORM_PACK16                          = 7,
       VK_FORMAT_A1R5G5B5_UNORM_PACK16                          = 8,
       VK_FORMAT_R8_UNORM                                       = 9,
       VK_FORMAT_R8_SNORM                                       = 10,
       VK_FORMAT_R8_USCALED                                     = 11,
       VK_FORMAT_R8_SSCALED                                     = 12,
       VK_FORMAT_R8_UINT                                        = 13,
       VK_FORMAT_R8_SINT                                        = 14,
       VK_FORMAT_R8_SRGB                                        = 15,
       VK_FORMAT_R8G8_UNORM                                     = 16,
       VK_FORMAT_R8G8_SNORM                                     = 17,
       VK_FORMAT_R8G8_USCALED                                   = 18,
       VK_FORMAT_R8G8_SSCALED                                   = 19,
       VK_FORMAT_R8G8_UINT                                      = 20,
       VK_FORMAT_R8G8_SINT                                      = 21,
       VK_FORMAT_R8G8_SRGB                                      = 22,
       VK_FORMAT_R8G8B8_UNORM                                   = 23,
       VK_FORMAT_R8G8B8_SNORM                                   = 24,
       VK_FORMAT_R8G8B8_USCALED                                 = 25,
       VK_FORMAT_R8G8B8_SSCALED                                 = 26,
       VK_FORMAT_R8G8B8_UINT                                    = 27,
       VK_FORMAT_R8G8B8_SINT                                    = 28,
       VK_FORMAT_R8G8B8_SRGB                                    = 29,
       VK_FORMAT_B8G8R8_UNORM                                   = 30,
       VK_FORMAT_B8G8R8_SNORM                                   = 31,
       VK_FORMAT_B8G8R8_USCALED                                 = 32,
       VK_FORMAT_B8G8R8_SSCALED                                 = 33,
       VK_FORMAT_B8G8R8_UINT                                    = 34,
       VK_FORMAT_B8G8R8_SINT                                    = 35,
       VK_FORMAT_B8G8R8_SRGB                                    = 36,
       VK_FORMAT_R8G8B8A8_UNORM                                 = 37,
       VK_FORMAT_R8G8B8A8_SNORM                                 = 38,
       VK_FORMAT_R8G8B8A8_USCALED                               = 39,
       VK_FORMAT_R8G8B8A8_SSCALED                               = 40,
       VK_FORMAT_R8G8B8A8_UINT                                  = 41,
       VK_FORMAT_R8G8B8A8_SINT                                  = 42,
       VK_FORMAT_R8G8B8A8_SRGB                                  = 43,
       VK_FORMAT_B8G8R8A8_UNORM                                 = 44,
       VK_FORMAT_B8G8R8A8_SNORM                                 = 45,
       VK_FORMAT_B8G8R8A8_USCALED                               = 46,
       VK_FORMAT_B8G8R8A8_SSCALED                               = 47,
       VK_FORMAT_B8G8R8A8_UINT                                  = 48,
       VK_FORMAT_B8G8R8A8_SINT                                  = 49,
       VK_FORMAT_B8G8R8A8_SRGB                                  = 50,
       VK_FORMAT_A8B8G8R8_UNORM_PACK32                          = 51,
       VK_FORMAT_A8B8G8R8_SNORM_PACK32                          = 52,
       VK_FORMAT_A8B8G8R8_USCALED_PACK32                        = 53,
       VK_FORMAT_A8B8G8R8_SSCALED_PACK32                        = 54,
       VK_FORMAT_A8B8G8R8_UINT_PACK32                           = 55,
       VK_FORMAT_A8B8G8R8_SINT_PACK32                           = 56,
       VK_FORMAT_A8B8G8R8_SRGB_PACK32                           = 57,
       VK_FORMAT_A2R10G10B10_UNORM_PACK32                       = 58,
       VK_FORMAT_A2R10G10B10_SNORM_PACK32                       = 59,
       VK_FORMAT_A2R10G10B10_USCALED_PACK32                     = 60,
       VK_FORMAT_A2R10G10B10_SSCALED_PACK32                     = 61,
       VK_FORMAT_A2R10G10B10_UINT_PACK32                        = 62,
       VK_FORMAT_A2R10G10B10_SINT_PACK32                        = 63,
       VK_FORMAT_A2B10G10R10_UNORM_PACK32                       = 64,
       VK_FORMAT_A2B10G10R10_SNORM_PACK32                       = 65,
       VK_FORMAT_A2B10G10R10_USCALED_PACK32                     = 66,
       VK_FORMAT_A2B10G10R10_SSCALED_PACK32                     = 67,
       VK_FORMAT_A2B10G10R10_UINT_PACK32                        = 68,
       VK_FORMAT_A2B10G10R10_SINT_PACK32                        = 69,
       VK_FORMAT_R16_UNORM                                      = 70,
       VK_FORMAT_R16_SNORM                                      = 71,
       VK_FORMAT_R16_USCALED                                    = 72,
       VK_FORMAT_R16_SSCALED                                    = 73,
       VK_FORMAT_R16_UINT                                       = 74,
       VK_FORMAT_R16_SINT                                       = 75,
       VK_FORMAT_R16_SFLOAT                                     = 76,
       VK_FORMAT_R16G16_UNORM                                   = 77,
       VK_FORMAT_R16G16_SNORM                                   = 78,
       VK_FORMAT_R16G16_USCALED                                 = 79,
       VK_FORMAT_R16G16_SSCALED                                 = 80,
       VK_FORMAT_R16G16_UINT                                    = 81,
       VK_FORMAT_R16G16_SINT                                    = 82,
       VK_FORMAT_R16G16_SFLOAT                                  = 83,
       VK_FORMAT_R16G16B16_UNORM                                = 84,
       VK_FORMAT_R16G16B16_SNORM                                = 85,
       VK_FORMAT_R16G16B16_USCALED                              = 86,
       VK_FORMAT_R16G16B16_SSCALED                              = 87,
       VK_FORMAT_R16G16B16_UINT                                 = 88,
       VK_FORMAT_R16G16B16_SINT                                 = 89,
       VK_FORMAT_R16G16B16_SFLOAT                               = 90,
       VK_FORMAT_R16G16B16A16_UNORM                             = 91,
       VK_FORMAT_R16G16B16A16_SNORM                             = 92,
       VK_FORMAT_R16G16B16A16_USCALED                           = 93,
       VK_FORMAT_R16G16B16A16_SSCALED                           = 94,
       VK_FORMAT_R16G16B16A16_UINT                              = 95,
       VK_FORMAT_R16G16B16A16_SINT                              = 96,
       VK_FORMAT_R16G16B16A16_SFLOAT                            = 97,
       VK_FORMAT_R32_UINT                                       = 98,
       VK_FORMAT_R32_SINT                                       = 99,
       VK_FORMAT_R32_SFLOAT                                     = 100,
       VK_FORMAT_R32G32_UINT                                    = 101,
       VK_FORMAT_R32G32_SINT                                    = 102,
       VK_FORMAT_R32G32_SFLOAT                                  = 103,
       VK_FORMAT_R32G32B32_UINT                                 = 104,
       VK_FORMAT_R32G32B32_SINT                                 = 105,
       VK_FORMAT_R32G32B32_SFLOAT                               = 106,
       VK_FORMAT_R32G32B32A32_UINT                              = 107,
       VK_FORMAT_R32G32B32A32_SINT                              = 108,
       VK_FORMAT_R32G32B32A32_SFLOAT                            = 109,
       VK_FORMAT_R64_UINT                                       = 110,
       VK_FORMAT_R64_SINT                                       = 111,
       VK_FORMAT_R64_SFLOAT                                     = 112,
       VK_FORMAT_R64G64_UINT                                    = 113,
       VK_FORMAT_R64G64_SINT                                    = 114,
       VK_FORMAT_R64G64_SFLOAT                                  = 115,
       VK_FORMAT_R64G64B64_UINT                                 = 116,
       VK_FORMAT_R64G64B64_SINT                                 = 117,
       VK_FORMAT_R64G64B64_SFLOAT                               = 118,
       VK_FORMAT_R64G64B64A64_UINT                              = 119,
       VK_FORMAT_R64G64B64A64_SINT                              = 120,
       VK_FORMAT_R64G64B64A64_SFLOAT                            = 121,
       VK_FORMAT_B10G11R11_UFLOAT_PACK32                        = 122,
       VK_FORMAT_E5B9G9R9_UFLOAT_PACK32                         = 123,
       VK_FORMAT_D16_UNORM                                      = 124,
       VK_FORMAT_X8_D24_UNORM_PACK32                            = 125,
       VK_FORMAT_D32_SFLOAT                                     = 126,
       VK_FORMAT_S8_UINT                                        = 127,
       VK_FORMAT_D16_UNORM_S8_UINT                              = 128,
       VK_FORMAT_D24_UNORM_S8_UINT                              = 129,
       VK_FORMAT_D32_SFLOAT_S8_UINT                             = 130,
       VK_FORMAT_BC1_RGB_UNORM_BLOCK                            = 131,
       VK_FORMAT_BC1_RGB_SRGB_BLOCK                             = 132,
       VK_FORMAT_BC1_RGBA_UNORM_BLOCK                           = 133,
       VK_FORMAT_BC1_RGBA_SRGB_BLOCK                            = 134,
       VK_FORMAT_BC2_UNORM_BLOCK                                = 135,
       VK_FORMAT_BC2_SRGB_BLOCK                                 = 136,
       VK_FORMAT_BC3_UNORM_BLOCK                                = 137,
       VK_FORMAT_BC3_SRGB_BLOCK                                 = 138,
       VK_FORMAT_BC4_UNORM_BLOCK                                = 139,
       VK_FORMAT_BC4_SNORM_BLOCK                                = 140,
       VK_FORMAT_BC5_UNORM_BLOCK                                = 141,
       VK_FORMAT_BC5_SNORM_BLOCK                                = 142,
       VK_FORMAT_BC6H_UFLOAT_BLOCK                              = 143,
       VK_FORMAT_BC6H_SFLOAT_BLOCK                              = 144,
       VK_FORMAT_BC7_UNORM_BLOCK                                = 145,
       VK_FORMAT_BC7_SRGB_BLOCK                                 = 146,
       VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK                        = 147,
       VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK                         = 148,
       VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK                      = 149,
       VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK                       = 150,
       VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK                      = 151,
       VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK                       = 152,
       VK_FORMAT_EAC_R11_UNORM_BLOCK                            = 153,
       VK_FORMAT_EAC_R11_SNORM_BLOCK                            = 154,
       VK_FORMAT_EAC_R11G11_UNORM_BLOCK                         = 155,
       VK_FORMAT_EAC_R11G11_SNORM_BLOCK                         = 156,
       VK_FORMAT_ASTC_4x4_UNORM_BLOCK                           = 157,
       VK_FORMAT_ASTC_4x4_SRGB_BLOCK                            = 158,
       VK_FORMAT_ASTC_5x4_UNORM_BLOCK                           = 159,
       VK_FORMAT_ASTC_5x4_SRGB_BLOCK                            = 160,
       VK_FORMAT_ASTC_5x5_UNORM_BLOCK                           = 161,
       VK_FORMAT_ASTC_5x5_SRGB_BLOCK                            = 162,
       VK_FORMAT_ASTC_6x5_UNORM_BLOCK                           = 163,
       VK_FORMAT_ASTC_6x5_SRGB_BLOCK                            = 164,
       VK_FORMAT_ASTC_6x6_UNORM_BLOCK                           = 165,
       VK_FORMAT_ASTC_6x6_SRGB_BLOCK                            = 166,
       VK_FORMAT_ASTC_8x5_UNORM_BLOCK                           = 167,
       VK_FORMAT_ASTC_8x5_SRGB_BLOCK                            = 168,
       VK_FORMAT_ASTC_8x6_UNORM_BLOCK                           = 169,
       VK_FORMAT_ASTC_8x6_SRGB_BLOCK                            = 170,
       VK_FORMAT_ASTC_8x8_UNORM_BLOCK                           = 171,
       VK_FORMAT_ASTC_8x8_SRGB_BLOCK                            = 172,
       VK_FORMAT_ASTC_10x5_UNORM_BLOCK                          = 173,
       VK_FORMAT_ASTC_10x5_SRGB_BLOCK                           = 174,
       VK_FORMAT_ASTC_10x6_UNORM_BLOCK                          = 175,
       VK_FORMAT_ASTC_10x6_SRGB_BLOCK                           = 176,
       VK_FORMAT_ASTC_10x8_UNORM_BLOCK                          = 177,
       VK_FORMAT_ASTC_10x8_SRGB_BLOCK                           = 178,
       VK_FORMAT_ASTC_10x10_UNORM_BLOCK                         = 179,
       VK_FORMAT_ASTC_10x10_SRGB_BLOCK                          = 180,
       VK_FORMAT_ASTC_12x10_UNORM_BLOCK                         = 181,
       VK_FORMAT_ASTC_12x10_SRGB_BLOCK                          = 182,
       VK_FORMAT_ASTC_12x12_UNORM_BLOCK                         = 183,
       VK_FORMAT_ASTC_12x12_SRGB_BLOCK                          = 184,
       VK_FORMAT_G8B8G8R8_422_UNORM                             = 1000156000,
       VK_FORMAT_B8G8R8G8_422_UNORM                             = 1000156001,
       VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM                      = 1000156002,
       VK_FORMAT_G8_B8R8_2PLANE_420_UNORM                       = 1000156003,
       VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM                      = 1000156004,
       VK_FORMAT_G8_B8R8_2PLANE_422_UNORM                       = 1000156005,
       VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM                      = 1000156006,
       VK_FORMAT_R10X6_UNORM_PACK16                             = 1000156007,
       VK_FORMAT_R10X6G10X6_UNORM_2PACK16                       = 1000156008,
       VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16             = 1000156009,
       VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16         = 1000156010,
       VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16         = 1000156011,
       VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16     = 1000156012,
       VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16      = 1000156013,
       VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16     = 1000156014,
       VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16      = 1000156015,
       VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16     = 1000156016,
       VK_FORMAT_R12X4_UNORM_PACK16                             = 1000156017,
       VK_FORMAT_R12X4G12X4_UNORM_2PACK16                       = 1000156018,
       VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16             = 1000156019,
       VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16         = 1000156020,
       VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16         = 1000156021,
       VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16     = 1000156022,
       VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16      = 1000156023,
       VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16     = 1000156024,
       VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16      = 1000156025,
       VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16     = 1000156026,
       VK_FORMAT_G16B16G16R16_422_UNORM                         = 1000156027,
       VK_FORMAT_B16G16R16G16_422_UNORM                         = 1000156028,
       VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM                   = 1000156029,
       VK_FORMAT_G16_B16R16_2PLANE_420_UNORM                    = 1000156030,
       VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM                   = 1000156031,
       VK_FORMAT_G16_B16R16_2PLANE_422_UNORM                    = 1000156032,
       VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM                   = 1000156033,
       VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG                    = 1000054000,
       VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG                    = 1000054001,
       VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG                    = 1000054002,
       VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG                    = 1000054003,
       VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG                     = 1000054004,
       VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG                     = 1000054005,
       VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG                     = 1000054006,
       VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG                     = 1000054007,
       VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT                      = 1000066000,
       VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT                      = 1000066001,
       VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT                      = 1000066002,
       VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT                      = 1000066003,
       VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT                      = 1000066004,
       VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT                      = 1000066005,
       VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT                      = 1000066006,
       VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT                      = 1000066007,
       VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT                     = 1000066008,
       VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT                     = 1000066009,
       VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT                     = 1000066010,
       VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT                    = 1000066011,
       VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT                    = 1000066012,
       VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT                    = 1000066013,
       VK_FORMAT_A4R4G4B4_UNORM_PACK16_EXT                      = 1000340000,
       VK_FORMAT_A4B4G4R4_UNORM_PACK16_EXT                      = 1000340001,
       VK_FORMAT_G8B8G8R8_422_UNORM_KHR                         = VK_FORMAT_G8B8G8R8_422_UNORM,
       VK_FORMAT_B8G8R8G8_422_UNORM_KHR                         = VK_FORMAT_B8G8R8G8_422_UNORM,
       VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR                  = VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM,
       VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR                   = VK_FORMAT_G8_B8R8_2PLANE_420_UNORM,
       VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR                  = VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM,
       VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR                   = VK_FORMAT_G8_B8R8_2PLANE_422_UNORM,
       VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR                  = VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM,
       VK_FORMAT_R10X6_UNORM_PACK16_KHR                         = VK_FORMAT_R10X6_UNORM_PACK16,
       VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR                   = VK_FORMAT_R10X6G10X6_UNORM_2PACK16,
       VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR         = VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16,
       VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR     = VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16,
       VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR     = VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16,
       VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16,
       VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR  = VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16,
       VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16,
       VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR  = VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16,
       VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16,
       VK_FORMAT_R12X4_UNORM_PACK16_KHR                         = VK_FORMAT_R12X4_UNORM_PACK16,
       VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR                   = VK_FORMAT_R12X4G12X4_UNORM_2PACK16,
       VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR         = VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16,
       VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR     = VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16,
       VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR     = VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16,
       VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16,
       VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR  = VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16,
       VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16,
       VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR  = VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16,
       VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16,
       VK_FORMAT_G16B16G16R16_422_UNORM_KHR                     = VK_FORMAT_G16B16G16R16_422_UNORM,
       VK_FORMAT_B16G16R16G16_422_UNORM_KHR                     = VK_FORMAT_B16G16R16G16_422_UNORM,
       VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR               = VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM,
       VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR                = VK_FORMAT_G16_B16R16_2PLANE_420_UNORM,
       VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR               = VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM,
       VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR                = VK_FORMAT_G16_B16R16_2PLANE_422_UNORM,
       VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR               = VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM,
       VK_FORMAT_MAX_ENUM                                       = $7FFFFFFF
     );

type P_VkImageTiling = ^VkImageTiling;
     VkImageTiling = (
       VK_IMAGE_TILING_OPTIMAL                 = 0,
       VK_IMAGE_TILING_LINEAR                  = 1,
       VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT = 1000158000,
       VK_IMAGE_TILING_MAX_ENUM                = $7FFFFFFF
     );

type P_VkImageType = ^VkImageType;
     VkImageType = (
       VK_IMAGE_TYPE_1D       = 0,
       VK_IMAGE_TYPE_2D       = 1,
       VK_IMAGE_TYPE_3D       = 2,
       VK_IMAGE_TYPE_MAX_ENUM = $7FFFFFFF
     );

type P_VkPhysicalDeviceType = ^VkPhysicalDeviceType;
     VkPhysicalDeviceType = (
       VK_PHYSICAL_DEVICE_TYPE_OTHER          = 0,
       VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = 1,
       VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   = 2,
       VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    = 3,
       VK_PHYSICAL_DEVICE_TYPE_CPU            = 4,
       VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM       = $7FFFFFFF
     );

type P_VkQueryType = ^VkQueryType;
     VkQueryType = (
       VK_QUERY_TYPE_OCCLUSION                                     = 0,
       VK_QUERY_TYPE_PIPELINE_STATISTICS                           = 1,
       VK_QUERY_TYPE_TIMESTAMP                                     = 2,
       VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT                 = 1000028004,
       VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR                         = 1000116000,
       VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR     = 1000150000,
       VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR = 1000150001,
       VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV      = 1000165000,
       VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL                       = 1000210000,
       VK_QUERY_TYPE_MAX_ENUM                                      = $7FFFFFFF
     );

type P_VkSharingMode = ^VkSharingMode;
     VkSharingMode = (
       VK_SHARING_MODE_EXCLUSIVE  = 0,
       VK_SHARING_MODE_CONCURRENT = 1,
       VK_SHARING_MODE_MAX_ENUM   = $7FFFFFFF
     );

type P_VkComponentSwizzle = ^VkComponentSwizzle;
     VkComponentSwizzle = (
       VK_COMPONENT_SWIZZLE_IDENTITY = 0,
       VK_COMPONENT_SWIZZLE_ZERO     = 1,
       VK_COMPONENT_SWIZZLE_ONE      = 2,
       VK_COMPONENT_SWIZZLE_R        = 3,
       VK_COMPONENT_SWIZZLE_G        = 4,
       VK_COMPONENT_SWIZZLE_B        = 5,
       VK_COMPONENT_SWIZZLE_A        = 6,
       VK_COMPONENT_SWIZZLE_MAX_ENUM = $7FFFFFFF
     );

type P_VkImageViewType = ^VkImageViewType;
     VkImageViewType = (
       VK_IMAGE_VIEW_TYPE_1D         = 0,
       VK_IMAGE_VIEW_TYPE_2D         = 1,
       VK_IMAGE_VIEW_TYPE_3D         = 2,
       VK_IMAGE_VIEW_TYPE_CUBE       = 3,
       VK_IMAGE_VIEW_TYPE_1D_ARRAY   = 4,
       VK_IMAGE_VIEW_TYPE_2D_ARRAY   = 5,
       VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = 6,
       VK_IMAGE_VIEW_TYPE_MAX_ENUM   = $7FFFFFFF
     );

type P_VkBlendFactor = ^VkBlendFactor;
     VkBlendFactor = (
       VK_BLEND_FACTOR_ZERO                     = 0,
       VK_BLEND_FACTOR_ONE                      = 1,
       VK_BLEND_FACTOR_SRC_COLOR                = 2,
       VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR      = 3,
       VK_BLEND_FACTOR_DST_COLOR                = 4,
       VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR      = 5,
       VK_BLEND_FACTOR_SRC_ALPHA                = 6,
       VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA      = 7,
       VK_BLEND_FACTOR_DST_ALPHA                = 8,
       VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA      = 9,
       VK_BLEND_FACTOR_CONSTANT_COLOR           = 10,
       VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = 11,
       VK_BLEND_FACTOR_CONSTANT_ALPHA           = 12,
       VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = 13,
       VK_BLEND_FACTOR_SRC_ALPHA_SATURATE       = 14,
       VK_BLEND_FACTOR_SRC1_COLOR               = 15,
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR     = 16,
       VK_BLEND_FACTOR_SRC1_ALPHA               = 17,
       VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA     = 18,
       VK_BLEND_FACTOR_MAX_ENUM                 = $7FFFFFFF
     );

type P_VkBlendOp = ^VkBlendOp;
     VkBlendOp = (
       VK_BLEND_OP_ADD                    = 0,
       VK_BLEND_OP_SUBTRACT               = 1,
       VK_BLEND_OP_REVERSE_SUBTRACT       = 2,
       VK_BLEND_OP_MIN                    = 3,
       VK_BLEND_OP_MAX                    = 4,
       VK_BLEND_OP_ZERO_EXT               = 1000148000,
       VK_BLEND_OP_SRC_EXT                = 1000148001,
       VK_BLEND_OP_DST_EXT                = 1000148002,
       VK_BLEND_OP_SRC_OVER_EXT           = 1000148003,
       VK_BLEND_OP_DST_OVER_EXT           = 1000148004,
       VK_BLEND_OP_SRC_IN_EXT             = 1000148005,
       VK_BLEND_OP_DST_IN_EXT             = 1000148006,
       VK_BLEND_OP_SRC_OUT_EXT            = 1000148007,
       VK_BLEND_OP_DST_OUT_EXT            = 1000148008,
       VK_BLEND_OP_SRC_ATOP_EXT           = 1000148009,
       VK_BLEND_OP_DST_ATOP_EXT           = 1000148010,
       VK_BLEND_OP_XOR_EXT                = 1000148011,
       VK_BLEND_OP_MULTIPLY_EXT           = 1000148012,
       VK_BLEND_OP_SCREEN_EXT             = 1000148013,
       VK_BLEND_OP_OVERLAY_EXT            = 1000148014,
       VK_BLEND_OP_DARKEN_EXT             = 1000148015,
       VK_BLEND_OP_LIGHTEN_EXT            = 1000148016,
       VK_BLEND_OP_COLORDODGE_EXT         = 1000148017,
       VK_BLEND_OP_COLORBURN_EXT          = 1000148018,
       VK_BLEND_OP_HARDLIGHT_EXT          = 1000148019,
       VK_BLEND_OP_SOFTLIGHT_EXT          = 1000148020,
       VK_BLEND_OP_DIFFERENCE_EXT         = 1000148021,
       VK_BLEND_OP_EXCLUSION_EXT          = 1000148022,
       VK_BLEND_OP_INVERT_EXT             = 1000148023,
       VK_BLEND_OP_INVERT_RGB_EXT         = 1000148024,
       VK_BLEND_OP_LINEARDODGE_EXT        = 1000148025,
       VK_BLEND_OP_LINEARBURN_EXT         = 1000148026,
       VK_BLEND_OP_VIVIDLIGHT_EXT         = 1000148027,
       VK_BLEND_OP_LINEARLIGHT_EXT        = 1000148028,
       VK_BLEND_OP_PINLIGHT_EXT           = 1000148029,
       VK_BLEND_OP_HARDMIX_EXT            = 1000148030,
       VK_BLEND_OP_HSL_HUE_EXT            = 1000148031,
       VK_BLEND_OP_HSL_SATURATION_EXT     = 1000148032,
       VK_BLEND_OP_HSL_COLOR_EXT          = 1000148033,
       VK_BLEND_OP_HSL_LUMINOSITY_EXT     = 1000148034,
       VK_BLEND_OP_PLUS_EXT               = 1000148035,
       VK_BLEND_OP_PLUS_CLAMPED_EXT       = 1000148036,
       VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = 1000148037,
       VK_BLEND_OP_PLUS_DARKER_EXT        = 1000148038,
       VK_BLEND_OP_MINUS_EXT              = 1000148039,
       VK_BLEND_OP_MINUS_CLAMPED_EXT      = 1000148040,
       VK_BLEND_OP_CONTRAST_EXT           = 1000148041,
       VK_BLEND_OP_INVERT_OVG_EXT         = 1000148042,
       VK_BLEND_OP_RED_EXT                = 1000148043,
       VK_BLEND_OP_GREEN_EXT              = 1000148044,
       VK_BLEND_OP_BLUE_EXT               = 1000148045,
       VK_BLEND_OP_MAX_ENUM               = $7FFFFFFF
     );

type P_VkCompareOp = ^VkCompareOp;
     VkCompareOp = (
       VK_COMPARE_OP_NEVER            = 0,
       VK_COMPARE_OP_LESS             = 1,
       VK_COMPARE_OP_EQUAL            = 2,
       VK_COMPARE_OP_LESS_OR_EQUAL    = 3,
       VK_COMPARE_OP_GREATER          = 4,
       VK_COMPARE_OP_NOT_EQUAL        = 5,
       VK_COMPARE_OP_GREATER_OR_EQUAL = 6,
       VK_COMPARE_OP_ALWAYS           = 7,
       VK_COMPARE_OP_MAX_ENUM         = $7FFFFFFF
     );

type P_VkDynamicState = ^VkDynamicState;
     VkDynamicState = (
       VK_DYNAMIC_STATE_VIEWPORT                            = 0,
       VK_DYNAMIC_STATE_SCISSOR                             = 1,
       VK_DYNAMIC_STATE_LINE_WIDTH                          = 2,
       VK_DYNAMIC_STATE_DEPTH_BIAS                          = 3,
       VK_DYNAMIC_STATE_BLEND_CONSTANTS                     = 4,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS                        = 5,
       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK                = 6,
       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK                  = 7,
       VK_DYNAMIC_STATE_STENCIL_REFERENCE                   = 8,
       VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV               = 1000087000,
       VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT               = 1000099000,
       VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT                = 1000143000,
       VK_DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR = 1000347000,
       VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV    = 1000164004,
       VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV     = 1000164006,
       VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV                = 1000205001,
       VK_DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR           = 1000226000,
       VK_DYNAMIC_STATE_LINE_STIPPLE_EXT                    = 1000259000,
       VK_DYNAMIC_STATE_CULL_MODE_EXT                       = 1000267000,
       VK_DYNAMIC_STATE_FRONT_FACE_EXT                      = 1000267001,
       VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT              = 1000267002,
       VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT             = 1000267003,
       VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT              = 1000267004,
       VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT     = 1000267005,
       VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT               = 1000267006,
       VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT              = 1000267007,
       VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT                = 1000267008,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT        = 1000267009,
       VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT             = 1000267010,
       VK_DYNAMIC_STATE_STENCIL_OP_EXT                      = 1000267011,
       VK_DYNAMIC_STATE_MAX_ENUM                            = $7FFFFFFF
     );

type P_VkFrontFace = ^VkFrontFace;
     VkFrontFace = (
       VK_FRONT_FACE_COUNTER_CLOCKWISE = 0,
       VK_FRONT_FACE_CLOCKWISE         = 1,
       VK_FRONT_FACE_MAX_ENUM          = $7FFFFFFF
     );

type P_VkVertexInputRate = ^VkVertexInputRate;
     VkVertexInputRate = (
       VK_VERTEX_INPUT_RATE_VERTEX   = 0,
       VK_VERTEX_INPUT_RATE_INSTANCE = 1,
       VK_VERTEX_INPUT_RATE_MAX_ENUM = $7FFFFFFF
     );

type P_VkPrimitiveTopology = ^VkPrimitiveTopology;
     VkPrimitiveTopology = (
       VK_PRIMITIVE_TOPOLOGY_POINT_LIST                    = 0,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST                     = 1,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP                    = 2,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST                 = 3,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP                = 4,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN                  = 5,
       VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY      = 6,
       VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY     = 7,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY  = 8,
       VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = 9,
       VK_PRIMITIVE_TOPOLOGY_PATCH_LIST                    = 10,
       VK_PRIMITIVE_TOPOLOGY_MAX_ENUM                      = $7FFFFFFF
     );

type P_VkPolygonMode = ^VkPolygonMode;
     VkPolygonMode = (
       VK_POLYGON_MODE_FILL              = 0,
       VK_POLYGON_MODE_LINE              = 1,
       VK_POLYGON_MODE_POINT             = 2,
       VK_POLYGON_MODE_FILL_RECTANGLE_NV = 1000153000,
       VK_POLYGON_MODE_MAX_ENUM          = $7FFFFFFF
     );

type P_VkStencilOp = ^VkStencilOp;
     VkStencilOp = (
       VK_STENCIL_OP_KEEP                = 0,
       VK_STENCIL_OP_ZERO                = 1,
       VK_STENCIL_OP_REPLACE             = 2,
       VK_STENCIL_OP_INCREMENT_AND_CLAMP = 3,
       VK_STENCIL_OP_DECREMENT_AND_CLAMP = 4,
       VK_STENCIL_OP_INVERT              = 5,
       VK_STENCIL_OP_INCREMENT_AND_WRAP  = 6,
       VK_STENCIL_OP_DECREMENT_AND_WRAP  = 7,
       VK_STENCIL_OP_MAX_ENUM            = $7FFFFFFF
     );

type P_VkLogicOp = ^VkLogicOp;
     VkLogicOp = (
       VK_LOGIC_OP_CLEAR         = 0,
       VK_LOGIC_OP_AND           = 1,
       VK_LOGIC_OP_AND_REVERSE   = 2,
       VK_LOGIC_OP_COPY          = 3,
       VK_LOGIC_OP_AND_INVERTED  = 4,
       VK_LOGIC_OP_NO_OP         = 5,
       VK_LOGIC_OP_XOR           = 6,
       VK_LOGIC_OP_OR            = 7,
       VK_LOGIC_OP_NOR           = 8,
       VK_LOGIC_OP_EQUIVALENT    = 9,
       VK_LOGIC_OP_INVERT        = 10,
       VK_LOGIC_OP_OR_REVERSE    = 11,
       VK_LOGIC_OP_COPY_INVERTED = 12,
       VK_LOGIC_OP_OR_INVERTED   = 13,
       VK_LOGIC_OP_NAND          = 14,
       VK_LOGIC_OP_SET           = 15,
       VK_LOGIC_OP_MAX_ENUM      = $7FFFFFFF
     );

type P_VkBorderColor = ^VkBorderColor;
     VkBorderColor = (
       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = 0,
       VK_BORDER_COLOR_INT_TRANSPARENT_BLACK   = 1,
       VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK      = 2,
       VK_BORDER_COLOR_INT_OPAQUE_BLACK        = 3,
       VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE      = 4,
       VK_BORDER_COLOR_INT_OPAQUE_WHITE        = 5,
       VK_BORDER_COLOR_FLOAT_CUSTOM_EXT        = 1000287003,
       VK_BORDER_COLOR_INT_CUSTOM_EXT          = 1000287004,
       VK_BORDER_COLOR_MAX_ENUM                = $7FFFFFFF
     );

type P_VkFilter = ^VkFilter;
     VkFilter = (
       VK_FILTER_NEAREST   = 0,
       VK_FILTER_LINEAR    = 1,
       VK_FILTER_CUBIC_IMG = 1000015000,
       VK_FILTER_CUBIC_EXT = VK_FILTER_CUBIC_IMG,
       VK_FILTER_MAX_ENUM  = $7FFFFFFF
     );

type P_VkSamplerAddressMode = ^VkSamplerAddressMode;
     VkSamplerAddressMode = (
       VK_SAMPLER_ADDRESS_MODE_REPEAT                   = 0,
       VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT          = 1,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE            = 2,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER          = 3,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE     = 4,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR = VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE,
       VK_SAMPLER_ADDRESS_MODE_MAX_ENUM                 = $7FFFFFFF
     );

type P_VkSamplerMipmapMode = ^VkSamplerMipmapMode;
     VkSamplerMipmapMode = (
       VK_SAMPLER_MIPMAP_MODE_NEAREST  = 0,
       VK_SAMPLER_MIPMAP_MODE_LINEAR   = 1,
       VK_SAMPLER_MIPMAP_MODE_MAX_ENUM = $7FFFFFFF
     );

type P_VkDescriptorType = ^VkDescriptorType;
     VkDescriptorType = (
       VK_DESCRIPTOR_TYPE_SAMPLER                    = 0,
       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER     = 1,
       VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE              = 2,
       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE              = 3,
       VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER       = 4,
       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER       = 5,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER             = 6,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER             = 7,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC     = 8,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC     = 9,
       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT           = 10,
       VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT   = 1000138000,
       VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR = 1000150000,
       VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV  = 1000165000,
       VK_DESCRIPTOR_TYPE_MUTABLE_VALVE              = 1000351000,
       VK_DESCRIPTOR_TYPE_MAX_ENUM                   = $7FFFFFFF
     );

type P_VkAttachmentLoadOp = ^VkAttachmentLoadOp;
     VkAttachmentLoadOp = (
       VK_ATTACHMENT_LOAD_OP_LOAD      = 0,
       VK_ATTACHMENT_LOAD_OP_CLEAR     = 1,
       VK_ATTACHMENT_LOAD_OP_DONT_CARE = 2,
       VK_ATTACHMENT_LOAD_OP_MAX_ENUM  = $7FFFFFFF
     );

type P_VkAttachmentStoreOp = ^VkAttachmentStoreOp;
     VkAttachmentStoreOp = (
       VK_ATTACHMENT_STORE_OP_STORE     = 0,
       VK_ATTACHMENT_STORE_OP_DONT_CARE = 1,
       VK_ATTACHMENT_STORE_OP_NONE_QCOM = 1000301000,
       VK_ATTACHMENT_STORE_OP_MAX_ENUM  = $7FFFFFFF
     );

type P_VkPipelineBindPoint = ^VkPipelineBindPoint;
     VkPipelineBindPoint = (
       VK_PIPELINE_BIND_POINT_GRAPHICS        = 0,
       VK_PIPELINE_BIND_POINT_COMPUTE         = 1,
       VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR = 1000165000,
       VK_PIPELINE_BIND_POINT_RAY_TRACING_NV  = VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR,
       VK_PIPELINE_BIND_POINT_MAX_ENUM        = $7FFFFFFF
     );

type P_VkCommandBufferLevel = ^VkCommandBufferLevel;
     VkCommandBufferLevel = (
       VK_COMMAND_BUFFER_LEVEL_PRIMARY   = 0,
       VK_COMMAND_BUFFER_LEVEL_SECONDARY = 1,
       VK_COMMAND_BUFFER_LEVEL_MAX_ENUM  = $7FFFFFFF
     );

type P_VkIndexType = ^VkIndexType;
     VkIndexType = (
       VK_INDEX_TYPE_UINT16    = 0,
       VK_INDEX_TYPE_UINT32    = 1,
       VK_INDEX_TYPE_NONE_KHR  = 1000165000,
       VK_INDEX_TYPE_UINT8_EXT = 1000265000,
       VK_INDEX_TYPE_NONE_NV   = VK_INDEX_TYPE_NONE_KHR,
       VK_INDEX_TYPE_MAX_ENUM  = $7FFFFFFF
     );

type P_VkSubpassContents = ^VkSubpassContents;
     VkSubpassContents = (
       VK_SUBPASS_CONTENTS_INLINE                    = 0,
       VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = 1,
       VK_SUBPASS_CONTENTS_MAX_ENUM                  = $7FFFFFFF
     );

type P_VkAccessFlagBits = ^VkAccessFlagBits;
     VkAccessFlagBits = (
       VK_ACCESS_INDIRECT_COMMAND_READ_BIT                     = $00000001,
       VK_ACCESS_INDEX_READ_BIT                                = $00000002,
       VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT                     = $00000004,
       VK_ACCESS_UNIFORM_READ_BIT                              = $00000008,
       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT                     = $00000010,
       VK_ACCESS_SHADER_READ_BIT                               = $00000020,
       VK_ACCESS_SHADER_WRITE_BIT                              = $00000040,
       VK_ACCESS_COLOR_ATTACHMENT_READ_BIT                     = $00000080,
       VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT                    = $00000100,
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT             = $00000200,
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT            = $00000400,
       VK_ACCESS_TRANSFER_READ_BIT                             = $00000800,
       VK_ACCESS_TRANSFER_WRITE_BIT                            = $00001000,
       VK_ACCESS_HOST_READ_BIT                                 = $00002000,
       VK_ACCESS_HOST_WRITE_BIT                                = $00004000,
       VK_ACCESS_MEMORY_READ_BIT                               = $00008000,
       VK_ACCESS_MEMORY_WRITE_BIT                              = $00010000,
       VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT              = $02000000,
       VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT       = $04000000,
       VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT      = $08000000,
       VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT            = $00100000,
       VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT     = $00080000,
       VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR           = $00200000,
       VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR          = $00400000,
       VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV                = $00800000,
       VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT             = $01000000,
       VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV                = $00020000,
       VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV               = $00040000,
       VK_ACCESS_NONE_KHR                                      = 0,
       VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV            = VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR,
       VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV           = VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR,
       VK_ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR = VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV,
       VK_ACCESS_FLAG_BITS_MAX_ENUM                            = $7FFFFFFF
     );
type P_VkAccessFlags = ^VkAccessFlags;
     VkAccessFlags = VkFlags;

type P_VkImageAspectFlagBits = ^VkImageAspectFlagBits;
     VkImageAspectFlagBits = (
       VK_IMAGE_ASPECT_COLOR_BIT              = $00000001,
       VK_IMAGE_ASPECT_DEPTH_BIT              = $00000002,
       VK_IMAGE_ASPECT_STENCIL_BIT            = $00000004,
       VK_IMAGE_ASPECT_METADATA_BIT           = $00000008,
       VK_IMAGE_ASPECT_PLANE_0_BIT            = $00000010,
       VK_IMAGE_ASPECT_PLANE_1_BIT            = $00000020,
       VK_IMAGE_ASPECT_PLANE_2_BIT            = $00000040,
       VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT = $00000080,
       VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT = $00000100,
       VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT = $00000200,
       VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT = $00000400,
       VK_IMAGE_ASPECT_PLANE_0_BIT_KHR        = VK_IMAGE_ASPECT_PLANE_0_BIT,
       VK_IMAGE_ASPECT_PLANE_1_BIT_KHR        = VK_IMAGE_ASPECT_PLANE_1_BIT,
       VK_IMAGE_ASPECT_PLANE_2_BIT_KHR        = VK_IMAGE_ASPECT_PLANE_2_BIT,
       VK_IMAGE_ASPECT_FLAG_BITS_MAX_ENUM     = $7FFFFFFF
     );
type P_VkImageAspectFlags = ^VkImageAspectFlags;
     VkImageAspectFlags = VkFlags;

type P_VkFormatFeatureFlagBits = ^VkFormatFeatureFlagBits;
     VkFormatFeatureFlagBits = (
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT                                                               = $00000001,
       VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT                                                               = $00000002,
       VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT                                                        = $00000004,
       VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT                                                        = $00000008,
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT                                                        = $00000010,
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT                                                 = $00000020,
       VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT                                                               = $00000040,
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT                                                            = $00000080,
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT                                                      = $00000100,
       VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT                                                    = $00000200,
       VK_FORMAT_FEATURE_BLIT_SRC_BIT                                                                    = $00000400,
       VK_FORMAT_FEATURE_BLIT_DST_BIT                                                                    = $00000800,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT                                                 = $00001000,
       VK_FORMAT_FEATURE_TRANSFER_SRC_BIT                                                                = $00004000,
       VK_FORMAT_FEATURE_TRANSFER_DST_BIT                                                                = $00008000,
       VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT                                                     = $00020000,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT                                = $00040000,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT               = $00080000,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT               = $00100000,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT     = $00200000,
       VK_FORMAT_FEATURE_DISJOINT_BIT                                                                    = $00400000,
       VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT                                                      = $00800000,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT                                                 = $00010000,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG                                              = $00002000,
       VK_FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR                                    = $20000000,
       VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT                                                    = $01000000,
       VK_FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR                                        = $40000000,
       VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR                                                            = VK_FORMAT_FEATURE_TRANSFER_SRC_BIT,
       VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR                                                            = VK_FORMAT_FEATURE_TRANSFER_DST_BIT,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT                                             = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT,
       VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR                                                 = VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR                            = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR           = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR           = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT,
       VK_FORMAT_FEATURE_DISJOINT_BIT_KHR                                                                = VK_FORMAT_FEATURE_DISJOINT_BIT,
       VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR                                                  = VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT                                              = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG,
       VK_FORMAT_FEATURE_FLAG_BITS_MAX_ENUM                                                              = $7FFFFFFF
     );
type P_VkFormatFeatureFlags = ^VkFormatFeatureFlags;
     VkFormatFeatureFlags = VkFlags;

type P_VkImageCreateFlagBits = ^VkImageCreateFlagBits;
     VkImageCreateFlagBits = (
       VK_IMAGE_CREATE_SPARSE_BINDING_BIT                        = $00000001,
       VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT                      = $00000002,
       VK_IMAGE_CREATE_SPARSE_ALIASED_BIT                        = $00000004,
       VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT                        = $00000008,
       VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT                       = $00000010,
       VK_IMAGE_CREATE_ALIAS_BIT                                 = $00000400,
       VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT           = $00000040,
       VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT                   = $00000020,
       VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT           = $00000080,
       VK_IMAGE_CREATE_EXTENDED_USAGE_BIT                        = $00000100,
       VK_IMAGE_CREATE_PROTECTED_BIT                             = $00000800,
       VK_IMAGE_CREATE_DISJOINT_BIT                              = $00000200,
       VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV                     = $00002000,
       VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = $00001000,
       VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT                        = $00004000,
       VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR       = VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT,
       VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR               = VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT,
       VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR       = VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT,
       VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR                    = VK_IMAGE_CREATE_EXTENDED_USAGE_BIT,
       VK_IMAGE_CREATE_DISJOINT_BIT_KHR                          = VK_IMAGE_CREATE_DISJOINT_BIT,
       VK_IMAGE_CREATE_ALIAS_BIT_KHR                             = VK_IMAGE_CREATE_ALIAS_BIT,
       VK_IMAGE_CREATE_FLAG_BITS_MAX_ENUM                        = $7FFFFFFF
     );
type P_VkImageCreateFlags = ^VkImageCreateFlags;
     VkImageCreateFlags = VkFlags;

type P_VkSampleCountFlagBits = ^VkSampleCountFlagBits;
     VkSampleCountFlagBits = (
       VK_SAMPLE_COUNT_1_BIT              = $00000001,
       VK_SAMPLE_COUNT_2_BIT              = $00000002,
       VK_SAMPLE_COUNT_4_BIT              = $00000004,
       VK_SAMPLE_COUNT_8_BIT              = $00000008,
       VK_SAMPLE_COUNT_16_BIT             = $00000010,
       VK_SAMPLE_COUNT_32_BIT             = $00000020,
       VK_SAMPLE_COUNT_64_BIT             = $00000040,
       VK_SAMPLE_COUNT_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkSampleCountFlags = ^VkSampleCountFlags;
     VkSampleCountFlags = VkFlags;

type P_VkImageUsageFlagBits = ^VkImageUsageFlagBits;
     VkImageUsageFlagBits = (
       VK_IMAGE_USAGE_TRANSFER_SRC_BIT                         = $00000001,
       VK_IMAGE_USAGE_TRANSFER_DST_BIT                         = $00000002,
       VK_IMAGE_USAGE_SAMPLED_BIT                              = $00000004,
       VK_IMAGE_USAGE_STORAGE_BIT                              = $00000008,
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT                     = $00000010,
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT             = $00000020,
       VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT                 = $00000040,
       VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT                     = $00000080,
       VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV                = $00000100,
       VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT             = $00000200,
       VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV,
       VK_IMAGE_USAGE_FLAG_BITS_MAX_ENUM                       = $7FFFFFFF
     );
type P_VkImageUsageFlags = ^VkImageUsageFlags;
     VkImageUsageFlags = VkFlags;
type P_VkInstanceCreateFlags = ^VkInstanceCreateFlags;
     VkInstanceCreateFlags = VkFlags;

type P_VkMemoryHeapFlagBits = ^VkMemoryHeapFlagBits;
     VkMemoryHeapFlagBits = (
       VK_MEMORY_HEAP_DEVICE_LOCAL_BIT       = $00000001,
       VK_MEMORY_HEAP_MULTI_INSTANCE_BIT     = $00000002,
       VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR = VK_MEMORY_HEAP_MULTI_INSTANCE_BIT,
       VK_MEMORY_HEAP_FLAG_BITS_MAX_ENUM     = $7FFFFFFF
     );
type P_VkMemoryHeapFlags = ^VkMemoryHeapFlags;
     VkMemoryHeapFlags = VkFlags;

type P_VkMemoryPropertyFlagBits = ^VkMemoryPropertyFlagBits;
     VkMemoryPropertyFlagBits = (
       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT        = $00000001,
       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT        = $00000002,
       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT       = $00000004,
       VK_MEMORY_PROPERTY_HOST_CACHED_BIT         = $00000008,
       VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT    = $00000010,
       VK_MEMORY_PROPERTY_PROTECTED_BIT           = $00000020,
       VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD = $00000040,
       VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD = $00000080,
       VK_MEMORY_PROPERTY_FLAG_BITS_MAX_ENUM      = $7FFFFFFF
     );
type P_VkMemoryPropertyFlags = ^VkMemoryPropertyFlags;
     VkMemoryPropertyFlags = VkFlags;

type P_VkQueueFlagBits = ^VkQueueFlagBits;
     VkQueueFlagBits = (
       VK_QUEUE_GRAPHICS_BIT       = $00000001,
       VK_QUEUE_COMPUTE_BIT        = $00000002,
       VK_QUEUE_TRANSFER_BIT       = $00000004,
       VK_QUEUE_SPARSE_BINDING_BIT = $00000008,
       VK_QUEUE_PROTECTED_BIT      = $00000010,
       VK_QUEUE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkQueueFlags = ^VkQueueFlags;
     VkQueueFlags = VkFlags;
type P_VkDeviceCreateFlags = ^VkDeviceCreateFlags;
     VkDeviceCreateFlags = VkFlags;

type P_VkDeviceQueueCreateFlagBits = ^VkDeviceQueueCreateFlagBits;
     VkDeviceQueueCreateFlagBits = (
       VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT      = $00000001,
       VK_DEVICE_QUEUE_CREATE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkDeviceQueueCreateFlags = ^VkDeviceQueueCreateFlags;
     VkDeviceQueueCreateFlags = VkFlags;

type P_VkPipelineStageFlagBits = ^VkPipelineStageFlagBits;
     VkPipelineStageFlagBits = (
       VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT                          = $00000001,
       VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT                        = $00000002,
       VK_PIPELINE_STAGE_VERTEX_INPUT_BIT                         = $00000004,
       VK_PIPELINE_STAGE_VERTEX_SHADER_BIT                        = $00000008,
       VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT          = $00000010,
       VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT       = $00000020,
       VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT                      = $00000040,
       VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT                      = $00000080,
       VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT                 = $00000100,
       VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT                  = $00000200,
       VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT              = $00000400,
       VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT                       = $00000800,
       VK_PIPELINE_STAGE_TRANSFER_BIT                             = $00001000,
       VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT                       = $00002000,
       VK_PIPELINE_STAGE_HOST_BIT                                 = $00004000,
       VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT                         = $00008000,
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT                         = $00010000,
       VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT               = $01000000,
       VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT            = $00040000,
       VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR     = $02000000,
       VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR               = $00200000,
       VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV                = $00400000,
       VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV                       = $00080000,
       VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV                       = $00100000,
       VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT         = $00800000,
       VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV                = $00020000,
       VK_PIPELINE_STAGE_NONE_KHR                                 = 0,
       VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV                = VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR,
       VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV      = VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR,
       VK_PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV,
       VK_PIPELINE_STAGE_FLAG_BITS_MAX_ENUM                       = $7FFFFFFF
     );
type VkPipelineStageFlags = VkFlags;  P_VkPipelineStageFlags = ^VkPipelineStageFlags;
type P_VkMemoryMapFlags = ^VkMemoryMapFlags;
     VkMemoryMapFlags = VkFlags;

type P_VkSparseMemoryBindFlagBits = ^VkSparseMemoryBindFlagBits;
     VkSparseMemoryBindFlagBits = (
       VK_SPARSE_MEMORY_BIND_METADATA_BIT       = $00000001,
       VK_SPARSE_MEMORY_BIND_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkSparseMemoryBindFlags = ^VkSparseMemoryBindFlags;
     VkSparseMemoryBindFlags = VkFlags;

type P_VkSparseImageFormatFlagBits = ^VkSparseImageFormatFlagBits;
     VkSparseImageFormatFlagBits = (
       VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT         = $00000001,
       VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT       = $00000002,
       VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = $00000004,
       VK_SPARSE_IMAGE_FORMAT_FLAG_BITS_MAX_ENUM         = $7FFFFFFF
     );
type P_VkSparseImageFormatFlags = ^VkSparseImageFormatFlags;
     VkSparseImageFormatFlags = VkFlags;

type P_VkFenceCreateFlagBits = ^VkFenceCreateFlagBits;
     VkFenceCreateFlagBits = (
       VK_FENCE_CREATE_SIGNALED_BIT       = $00000001,
       VK_FENCE_CREATE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkFenceCreateFlags = ^VkFenceCreateFlags;
     VkFenceCreateFlags = VkFlags;
type P_VkSemaphoreCreateFlags = ^VkSemaphoreCreateFlags;
     VkSemaphoreCreateFlags = VkFlags;

type P_VkEventCreateFlagBits = ^VkEventCreateFlagBits;
     VkEventCreateFlagBits = (
       VK_EVENT_CREATE_DEVICE_ONLY_BIT_KHR = $00000001,
       VK_EVENT_CREATE_FLAG_BITS_MAX_ENUM  = $7FFFFFFF
     );
type P_VkEventCreateFlags = ^VkEventCreateFlags;
     VkEventCreateFlags = VkFlags;

type P_VkQueryPipelineStatisticFlagBits = ^VkQueryPipelineStatisticFlagBits;
     VkQueryPipelineStatisticFlagBits = (
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT                    = $00000001,
       VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT                  = $00000002,
       VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT                  = $00000004,
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT                = $00000008,
       VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT                 = $00000010,
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT                       = $00000020,
       VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT                        = $00000040,
       VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT                = $00000080,
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT        = $00000100,
       VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = $00000200,
       VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT                 = $00000400,
       VK_QUERY_PIPELINE_STATISTIC_FLAG_BITS_MAX_ENUM                             = $7FFFFFFF
     );
type P_VkQueryPipelineStatisticFlags = ^VkQueryPipelineStatisticFlags;
     VkQueryPipelineStatisticFlags = VkFlags;
type P_VkQueryPoolCreateFlags = ^VkQueryPoolCreateFlags;
     VkQueryPoolCreateFlags = VkFlags;

type P_VkQueryResultFlagBits = ^VkQueryResultFlagBits;
     VkQueryResultFlagBits = (
       VK_QUERY_RESULT_64_BIT                = $00000001,
       VK_QUERY_RESULT_WAIT_BIT              = $00000002,
       VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = $00000004,
       VK_QUERY_RESULT_PARTIAL_BIT           = $00000008,
       VK_QUERY_RESULT_FLAG_BITS_MAX_ENUM    = $7FFFFFFF
     );
type P_VkQueryResultFlags = ^VkQueryResultFlags;
     VkQueryResultFlags = VkFlags;

type P_VkBufferCreateFlagBits = ^VkBufferCreateFlagBits;
     VkBufferCreateFlagBits = (
       VK_BUFFER_CREATE_SPARSE_BINDING_BIT                    = $00000001,
       VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT                  = $00000002,
       VK_BUFFER_CREATE_SPARSE_ALIASED_BIT                    = $00000004,
       VK_BUFFER_CREATE_PROTECTED_BIT                         = $00000008,
       VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT     = $00000010,
       VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
       VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR = VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
       VK_BUFFER_CREATE_FLAG_BITS_MAX_ENUM                    = $7FFFFFFF
     );
type P_VkBufferCreateFlags = ^VkBufferCreateFlags;
     VkBufferCreateFlags = VkFlags;

type P_VkBufferUsageFlagBits = ^VkBufferUsageFlagBits;
     VkBufferUsageFlagBits = (
       VK_BUFFER_USAGE_TRANSFER_SRC_BIT                                     = $00000001,
       VK_BUFFER_USAGE_TRANSFER_DST_BIT                                     = $00000002,
       VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT                             = $00000004,
       VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT                             = $00000008,
       VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT                                   = $00000010,
       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT                                   = $00000020,
       VK_BUFFER_USAGE_INDEX_BUFFER_BIT                                     = $00000040,
       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT                                    = $00000080,
       VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT                                  = $00000100,
       VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT                            = $00020000,
       VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT                    = $00000800,
       VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT            = $00001000,
       VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT                        = $00000200,
       VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR = $00080000,
       VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR               = $00100000,
       VK_BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR                         = $00000400,
       VK_BUFFER_USAGE_RAY_TRACING_BIT_NV                                   = VK_BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR,
       VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT                        = VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
       VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR                        = VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
       VK_BUFFER_USAGE_FLAG_BITS_MAX_ENUM                                   = $7FFFFFFF
     );
type P_VkBufferUsageFlags = ^VkBufferUsageFlags;
     VkBufferUsageFlags = VkFlags;
type P_VkBufferViewCreateFlags = ^VkBufferViewCreateFlags;
     VkBufferViewCreateFlags = VkFlags;

type P_VkImageViewCreateFlagBits = ^VkImageViewCreateFlagBits;
     VkImageViewCreateFlagBits = (
       VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT  = $00000001,
       VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT = $00000002,
       VK_IMAGE_VIEW_CREATE_FLAG_BITS_MAX_ENUM                    = $7FFFFFFF
     );
type P_VkImageViewCreateFlags = ^VkImageViewCreateFlags;
     VkImageViewCreateFlags = VkFlags;

type P_VkShaderModuleCreateFlagBits = ^VkShaderModuleCreateFlagBits;
     VkShaderModuleCreateFlagBits = (
       VK_SHADER_MODULE_CREATE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkShaderModuleCreateFlags = ^VkShaderModuleCreateFlags;
     VkShaderModuleCreateFlags = VkFlags;

type P_VkPipelineCacheCreateFlagBits = ^VkPipelineCacheCreateFlagBits;
     VkPipelineCacheCreateFlagBits = (
       VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT = $00000001,
       VK_PIPELINE_CACHE_CREATE_FLAG_BITS_MAX_ENUM              = $7FFFFFFF
     );
type P_VkPipelineCacheCreateFlags = ^VkPipelineCacheCreateFlags;
     VkPipelineCacheCreateFlags = VkFlags;

type P_VkColorComponentFlagBits = ^VkColorComponentFlagBits;
     VkColorComponentFlagBits = (
       VK_COLOR_COMPONENT_R_BIT              = $00000001,
       VK_COLOR_COMPONENT_G_BIT              = $00000002,
       VK_COLOR_COMPONENT_B_BIT              = $00000004,
       VK_COLOR_COMPONENT_A_BIT              = $00000008,
       VK_COLOR_COMPONENT_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkColorComponentFlags = ^VkColorComponentFlags;
     VkColorComponentFlags = VkFlags;

type P_VkPipelineCreateFlagBits = ^VkPipelineCreateFlagBits;
     VkPipelineCreateFlagBits = (
       VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT                               = $00000001,
       VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT                                  = $00000002,
       VK_PIPELINE_CREATE_DERIVATIVE_BIT                                         = $00000004,
       VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT                       = $00000008,
       VK_PIPELINE_CREATE_DISPATCH_BASE_BIT                                      = $00000010,
       VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR            = $00004000,
       VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR        = $00008000,
       VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR               = $00010000,
       VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR       = $00020000,
       VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR                     = $00001000,
       VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR                         = $00002000,
       VK_PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR = $00080000,
       VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV                                   = $00000020,
       VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR                             = $00000040,
       VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR               = $00000080,
       VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV                               = $00040000,
       VK_PIPELINE_CREATE_LIBRARY_BIT_KHR                                        = $00000800,
       VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT              = $00000100,
       VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT                        = $00000200,
       VK_PIPELINE_CREATE_DISPATCH_BASE                                          = VK_PIPELINE_CREATE_DISPATCH_BASE_BIT,
       VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR                   = VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT,
       VK_PIPELINE_CREATE_DISPATCH_BASE_KHR                                      = VK_PIPELINE_CREATE_DISPATCH_BASE,
       VK_PIPELINE_CREATE_FLAG_BITS_MAX_ENUM                                     = $7FFFFFFF
     );
type P_VkPipelineCreateFlags = ^VkPipelineCreateFlags;
     VkPipelineCreateFlags = VkFlags;

type P_VkPipelineShaderStageCreateFlagBits = ^VkPipelineShaderStageCreateFlagBits;
     VkPipelineShaderStageCreateFlagBits = (
       VK_PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT = $00000001,
       VK_PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT      = $00000002,
       VK_PIPELINE_SHADER_STAGE_CREATE_FLAG_BITS_MAX_ENUM                  = $7FFFFFFF
     );
type P_VkPipelineShaderStageCreateFlags = ^VkPipelineShaderStageCreateFlags;
     VkPipelineShaderStageCreateFlags = VkFlags;

type P_VkShaderStageFlagBits = ^VkShaderStageFlagBits;
     VkShaderStageFlagBits = (
       VK_SHADER_STAGE_VERTEX_BIT                  = $00000001,
       VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT    = $00000002,
       VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = $00000004,
       VK_SHADER_STAGE_GEOMETRY_BIT                = $00000008,
       VK_SHADER_STAGE_FRAGMENT_BIT                = $00000010,
       VK_SHADER_STAGE_COMPUTE_BIT                 = $00000020,
       VK_SHADER_STAGE_ALL_GRAPHICS                = $0000001F,
       VK_SHADER_STAGE_ALL                         = $7FFFFFFF,
       VK_SHADER_STAGE_RAYGEN_BIT_KHR              = $00000100,
       VK_SHADER_STAGE_ANY_HIT_BIT_KHR             = $00000200,
       VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR         = $00000400,
       VK_SHADER_STAGE_MISS_BIT_KHR                = $00000800,
       VK_SHADER_STAGE_INTERSECTION_BIT_KHR        = $00001000,
       VK_SHADER_STAGE_CALLABLE_BIT_KHR            = $00002000,
       VK_SHADER_STAGE_TASK_BIT_NV                 = $00000040,
       VK_SHADER_STAGE_MESH_BIT_NV                 = $00000080,
       VK_SHADER_STAGE_RAYGEN_BIT_NV               = VK_SHADER_STAGE_RAYGEN_BIT_KHR,
       VK_SHADER_STAGE_ANY_HIT_BIT_NV              = VK_SHADER_STAGE_ANY_HIT_BIT_KHR,
       VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV          = VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR,
       VK_SHADER_STAGE_MISS_BIT_NV                 = VK_SHADER_STAGE_MISS_BIT_KHR,
       VK_SHADER_STAGE_INTERSECTION_BIT_NV         = VK_SHADER_STAGE_INTERSECTION_BIT_KHR,
       VK_SHADER_STAGE_CALLABLE_BIT_NV             = VK_SHADER_STAGE_CALLABLE_BIT_KHR,
       VK_SHADER_STAGE_FLAG_BITS_MAX_ENUM          = $7FFFFFFF
     );

type P_VkCullModeFlagBits = ^VkCullModeFlagBits;
     VkCullModeFlagBits = (
       VK_CULL_MODE_NONE               = 0,
       VK_CULL_MODE_FRONT_BIT          = $00000001,
       VK_CULL_MODE_BACK_BIT           = $00000002,
       VK_CULL_MODE_FRONT_AND_BACK     = $00000003,
       VK_CULL_MODE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkCullModeFlags                         = ^VkCullModeFlags;
     VkCullModeFlags                         = VkFlags;
type P_VkPipelineVertexInputStateCreateFlags   = ^VkPipelineVertexInputStateCreateFlags;
     VkPipelineVertexInputStateCreateFlags   = VkFlags;
type P_VkPipelineInputAssemblyStateCreateFlags = ^VkPipelineInputAssemblyStateCreateFlags;
     VkPipelineInputAssemblyStateCreateFlags = VkFlags;
type P_VkPipelineTessellationStateCreateFlags  = ^VkPipelineTessellationStateCreateFlags;
     VkPipelineTessellationStateCreateFlags  = VkFlags;
type P_VkPipelineViewportStateCreateFlags      = ^VkPipelineViewportStateCreateFlags;
     VkPipelineViewportStateCreateFlags      = VkFlags;
type P_VkPipelineRasterizationStateCreateFlags = ^VkPipelineRasterizationStateCreateFlags;
     VkPipelineRasterizationStateCreateFlags = VkFlags;
type P_VkPipelineMultisampleStateCreateFlags   = ^VkPipelineMultisampleStateCreateFlags;
     VkPipelineMultisampleStateCreateFlags   = VkFlags;
type P_VkPipelineDepthStencilStateCreateFlags  = ^VkPipelineDepthStencilStateCreateFlags;
     VkPipelineDepthStencilStateCreateFlags  = VkFlags;
type P_VkPipelineColorBlendStateCreateFlags    = ^VkPipelineColorBlendStateCreateFlags;
     VkPipelineColorBlendStateCreateFlags    = VkFlags;
type P_VkPipelineDynamicStateCreateFlags       = ^VkPipelineDynamicStateCreateFlags;
     VkPipelineDynamicStateCreateFlags       = VkFlags;
type P_VkPipelineLayoutCreateFlags             = ^VkPipelineLayoutCreateFlags;
     VkPipelineLayoutCreateFlags             = VkFlags;
type P_VkShaderStageFlags                      = ^VkShaderStageFlags;
     VkShaderStageFlags                      = VkFlags;

type P_VkSamplerCreateFlagBits = ^VkSamplerCreateFlagBits;
     VkSamplerCreateFlagBits = (
       VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT                       = $00000001,
       VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = $00000002,
       VK_SAMPLER_CREATE_FLAG_BITS_MAX_ENUM                       = $7FFFFFFF
     );
type P_VkSamplerCreateFlags = ^VkSamplerCreateFlags;
     VkSamplerCreateFlags = VkFlags;

type P_VkDescriptorPoolCreateFlagBits = ^VkDescriptorPoolCreateFlagBits;
     VkDescriptorPoolCreateFlagBits = (
       VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT   = $00000001,
       VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT     = $00000002,
       VK_DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE       = $00000004,
       VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT,
       VK_DESCRIPTOR_POOL_CREATE_FLAG_BITS_MAX_ENUM        = $7FFFFFFF
     );
type P_VkDescriptorPoolCreateFlags = ^VkDescriptorPoolCreateFlags;
     VkDescriptorPoolCreateFlags = VkFlags;
type P_VkDescriptorPoolResetFlags = ^VkDescriptorPoolResetFlags;
     VkDescriptorPoolResetFlags = VkFlags;

type P_VkDescriptorSetLayoutCreateFlagBits = ^VkDescriptorSetLayoutCreateFlagBits;
     VkDescriptorSetLayoutCreateFlagBits = (
       VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT     = $00000002,
       VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR        = $00000001,
       VK_DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE       = $00000004,
       VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT,
       VK_DESCRIPTOR_SET_LAYOUT_CREATE_FLAG_BITS_MAX_ENUM             = $7FFFFFFF
     );
type P_VkDescriptorSetLayoutCreateFlags = ^VkDescriptorSetLayoutCreateFlags;
     VkDescriptorSetLayoutCreateFlags = VkFlags;

type P_VkAttachmentDescriptionFlagBits = ^VkAttachmentDescriptionFlagBits;
     VkAttachmentDescriptionFlagBits = (
       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT      = $00000001,
       VK_ATTACHMENT_DESCRIPTION_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkAttachmentDescriptionFlags = ^VkAttachmentDescriptionFlags;
     VkAttachmentDescriptionFlags = VkFlags;

type P_VkDependencyFlagBits = ^VkDependencyFlagBits;
     VkDependencyFlagBits = (
       VK_DEPENDENCY_BY_REGION_BIT        = $00000001,
       VK_DEPENDENCY_DEVICE_GROUP_BIT     = $00000004,
       VK_DEPENDENCY_VIEW_LOCAL_BIT       = $00000002,
       VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR   = VK_DEPENDENCY_VIEW_LOCAL_BIT,
       VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR = VK_DEPENDENCY_DEVICE_GROUP_BIT,
       VK_DEPENDENCY_FLAG_BITS_MAX_ENUM   = $7FFFFFFF
     );
type P_VkDependencyFlags = ^VkDependencyFlags;
     VkDependencyFlags = VkFlags;

type P_VkFramebufferCreateFlagBits = ^VkFramebufferCreateFlagBits;
     VkFramebufferCreateFlagBits = (
       VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT      = $00000001,
       VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR  = VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT,
       VK_FRAMEBUFFER_CREATE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkFramebufferCreateFlags = ^VkFramebufferCreateFlags;
     VkFramebufferCreateFlags = VkFlags;

type P_VkRenderPassCreateFlagBits = ^VkRenderPassCreateFlagBits;
     VkRenderPassCreateFlagBits = (
       VK_RENDER_PASS_CREATE_TRANSFORM_BIT_QCOM = $00000002,
       VK_RENDER_PASS_CREATE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkRenderPassCreateFlags = ^VkRenderPassCreateFlags;
     VkRenderPassCreateFlags = VkFlags;

type P_VkSubpassDescriptionFlagBits = ^VkSubpassDescriptionFlagBits;
     VkSubpassDescriptionFlagBits = (
       VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX      = $00000001,
       VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = $00000002,
       VK_SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM         = $00000004,
       VK_SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM          = $00000008,
       VK_SUBPASS_DESCRIPTION_FLAG_BITS_MAX_ENUM               = $7FFFFFFF
     );
type P_VkSubpassDescriptionFlags = ^VkSubpassDescriptionFlags;
     VkSubpassDescriptionFlags = VkFlags;

type P_VkCommandPoolCreateFlagBits = ^VkCommandPoolCreateFlagBits;
     VkCommandPoolCreateFlagBits = (
       VK_COMMAND_POOL_CREATE_TRANSIENT_BIT            = $00000001,
       VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = $00000002,
       VK_COMMAND_POOL_CREATE_PROTECTED_BIT            = $00000004,
       VK_COMMAND_POOL_CREATE_FLAG_BITS_MAX_ENUM       = $7FFFFFFF
     );
type P_VkCommandPoolCreateFlags = ^VkCommandPoolCreateFlags;
     VkCommandPoolCreateFlags = VkFlags;

type P_VkCommandPoolResetFlagBits = ^VkCommandPoolResetFlagBits;
     VkCommandPoolResetFlagBits = (
       VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = $00000001,
       VK_COMMAND_POOL_RESET_FLAG_BITS_MAX_ENUM    = $7FFFFFFF
     );
type P_VkCommandPoolResetFlags = ^VkCommandPoolResetFlags;
     VkCommandPoolResetFlags = VkFlags;

type P_VkCommandBufferUsageFlagBits = ^VkCommandBufferUsageFlagBits;
     VkCommandBufferUsageFlagBits = (
       VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT      = $00000001,
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = $00000002,
       VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT     = $00000004,
       VK_COMMAND_BUFFER_USAGE_FLAG_BITS_MAX_ENUM       = $7FFFFFFF
     );
type P_VkCommandBufferUsageFlags = ^VkCommandBufferUsageFlags;
     VkCommandBufferUsageFlags = VkFlags;

type P_VkQueryControlFlagBits = ^VkQueryControlFlagBits;
     VkQueryControlFlagBits = (
       VK_QUERY_CONTROL_PRECISE_BIT        = $00000001,
       VK_QUERY_CONTROL_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkQueryControlFlags = ^VkQueryControlFlags;
     VkQueryControlFlags = VkFlags;

type P_VkCommandBufferResetFlagBits = ^VkCommandBufferResetFlagBits;
     VkCommandBufferResetFlagBits = (
       VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = $00000001,
       VK_COMMAND_BUFFER_RESET_FLAG_BITS_MAX_ENUM    = $7FFFFFFF
     );
type P_VkCommandBufferResetFlags = ^VkCommandBufferResetFlags;
     VkCommandBufferResetFlags = VkFlags;

type P_VkStencilFaceFlagBits = ^VkStencilFaceFlagBits;
     VkStencilFaceFlagBits = (
       VK_STENCIL_FACE_FRONT_BIT          = $00000001,
       VK_STENCIL_FACE_BACK_BIT           = $00000002,
       VK_STENCIL_FACE_FRONT_AND_BACK     = $00000003,
       VK_STENCIL_FRONT_AND_BACK          = VK_STENCIL_FACE_FRONT_AND_BACK,
       VK_STENCIL_FACE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkStencilFaceFlags = ^VkStencilFaceFlags;
     VkStencilFaceFlags = VkFlags;
type P_VkExtent2D = ^VkExtent2D;
     VkExtent2D = record
       width  :T_uint32_t;
       height :T_uint32_t;
     end;

type P_VkExtent3D = ^VkExtent3D;
     VkExtent3D = record
       width  :T_uint32_t;
       height :T_uint32_t;
       depth  :T_uint32_t;
     end;

type P_VkOffset2D = ^VkOffset2D;
     VkOffset2D = record
       x :T_int32_t;
       y :T_int32_t;
     end;

type P_VkOffset3D = ^VkOffset3D;
     VkOffset3D = record
       x :T_int32_t;
       y :T_int32_t;
       z :T_int32_t;
     end;

type P_VkRect2D = ^VkRect2D;
     VkRect2D = record
       offset :VkOffset2D;
       extent :VkExtent2D;
     end;

type P_VkBaseInStructure = ^VkBaseInStructure;
     VkBaseInStructure = record
       sType :VkStructureType;
       pNext :P_VkBaseInStructure;
     end;

type P_VkBaseOutStructure = ^VkBaseOutStructure;
     VkBaseOutStructure = record
       sType :VkStructureType;
       pNext :P_VkBaseOutStructure;
     end;

type P_VkBufferMemoryBarrier = ^VkBufferMemoryBarrier;
     VkBufferMemoryBarrier = record
       sType               :VkStructureType;
       pNext               :P_void;
       srcAccessMask       :VkAccessFlags;
       dstAccessMask       :VkAccessFlags;
       srcQueueFamilyIndex :T_uint32_t;
       dstQueueFamilyIndex :T_uint32_t;
       buffer              :VkBuffer;
       offset              :VkDeviceSize;
       size                :VkDeviceSize;
     end;

type P_VkDispatchIndirectCommand = ^VkDispatchIndirectCommand;
     VkDispatchIndirectCommand = record
       x :T_uint32_t;
       y :T_uint32_t;
       z :T_uint32_t;
     end;

type P_VkDrawIndexedIndirectCommand = ^VkDrawIndexedIndirectCommand;
     VkDrawIndexedIndirectCommand = record
       indexCount    :T_uint32_t;
       instanceCount :T_uint32_t;
       firstIndex    :T_uint32_t;
       vertexOffset  :T_int32_t;
       firstInstance :T_uint32_t;
     end;

type P_VkDrawIndirectCommand = ^VkDrawIndirectCommand;
     VkDrawIndirectCommand = record
       vertexCount   :T_uint32_t;
       instanceCount :T_uint32_t;
       firstVertex   :T_uint32_t;
       firstInstance :T_uint32_t;
     end;

type P_VkImageSubresourceRange = ^VkImageSubresourceRange;
     VkImageSubresourceRange = record
       aspectMask     :VkImageAspectFlags;
       baseMipLevel   :T_uint32_t;
       levelCount     :T_uint32_t;
       baseArrayLayer :T_uint32_t;
       layerCount     :T_uint32_t;
     end;

type P_VkImageMemoryBarrier = ^VkImageMemoryBarrier;
     VkImageMemoryBarrier = record
       sType               :VkStructureType;
       pNext               :P_void;
       srcAccessMask       :VkAccessFlags;
       dstAccessMask       :VkAccessFlags;
       oldLayout           :VkImageLayout;
       newLayout           :VkImageLayout;
       srcQueueFamilyIndex :T_uint32_t;
       dstQueueFamilyIndex :T_uint32_t;
       image               :VkImage;
       subresourceRange    :VkImageSubresourceRange;
     end;

type P_VkMemoryBarrier = ^VkMemoryBarrier;
     VkMemoryBarrier = record
       sType         :VkStructureType;
       pNext         :P_void;
       srcAccessMask :VkAccessFlags;
       dstAccessMask :VkAccessFlags;
     end;

type PFN_vkAllocationFunction = function(
       pUserData_       :P_void;
       size_            :T_size_t;
       alignment_       :T_size_t;
       allocationScope_ :VkSystemAllocationScope ) :P_void;

type PFN_vkFreeFunction = procedure(
       pUserData_ :P_void;
       pMemory_   :P_void );

type PFN_vkInternalAllocationNotification = procedure(
       pUserData_       :P_void;
       size_            :T_size_t;
       allocationType_  :VkInternalAllocationType;
       allocationScope_ :VkSystemAllocationScope );

type PFN_vkInternalFreeNotification = procedure(
       pUserData_       :P_void;
       size_            :T_size_t;
       allocationType_  :VkInternalAllocationType;
       allocationScope_ :VkSystemAllocationScope );

type PFN_vkReallocationFunction = function(
       pUserData_       :P_void;
       pOriginal_       :P_void;
       size_            :T_size_t;
       alignment_       :T_size_t;
       allocationScope_ :VkSystemAllocationScope ) :P_void;

type P_PFN_vkVoidFunction = ^PFN_vkVoidFunction;
     PFN_vkVoidFunction = procedure;
type P_VkAllocationCallbacks = ^VkAllocationCallbacks;
     VkAllocationCallbacks = record
       pUserData             :P_void;
       pfnAllocation         :PFN_vkAllocationFunction;
       pfnReallocation       :PFN_vkReallocationFunction;
       pfnFree               :PFN_vkFreeFunction;
       pfnInternalAllocation :PFN_vkInternalAllocationNotification;
       pfnInternalFree       :PFN_vkInternalFreeNotification;
     end;

type P_VkApplicationInfo = ^VkApplicationInfo;
     VkApplicationInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       pApplicationName   :P_char;
       applicationVersion :T_uint32_t;
       pEngineName        :P_char;
       engineVersion      :T_uint32_t;
       apiVersion         :T_uint32_t;
     end;

type P_VkFormatProperties = ^VkFormatProperties;
     VkFormatProperties = record
       linearTilingFeatures  :VkFormatFeatureFlags;
       optimalTilingFeatures :VkFormatFeatureFlags;
       bufferFeatures        :VkFormatFeatureFlags;
     end;

type P_VkImageFormatProperties = ^VkImageFormatProperties;
     VkImageFormatProperties = record
       maxExtent       :VkExtent3D;
       maxMipLevels    :T_uint32_t;
       maxArrayLayers  :T_uint32_t;
       sampleCounts    :VkSampleCountFlags;
       maxResourceSize :VkDeviceSize;
     end;

type P_VkInstanceCreateInfo = ^VkInstanceCreateInfo;
     VkInstanceCreateInfo = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       flags                   :VkInstanceCreateFlags;
       pApplicationInfo        :P_VkApplicationInfo;
       enabledLayerCount       :T_uint32_t;
       ppEnabledLayerNames     :PP_char;
       enabledExtensionCount   :T_uint32_t;
       ppEnabledExtensionNames :PP_char;
     end;

type P_VkMemoryHeap = ^VkMemoryHeap;
     VkMemoryHeap = record
       size  :VkDeviceSize;
       flags :VkMemoryHeapFlags;
     end;

type P_VkMemoryType = ^VkMemoryType;
     VkMemoryType = record
       propertyFlags :VkMemoryPropertyFlags;
       heapIndex     :T_uint32_t;
     end;

type P_VkPhysicalDeviceFeatures = ^VkPhysicalDeviceFeatures;
     VkPhysicalDeviceFeatures = record
       robustBufferAccess                      :VkBool32;
       fullDrawIndexUint32                     :VkBool32;
       imageCubeArray                          :VkBool32;
       independentBlend                        :VkBool32;
       geometryShader                          :VkBool32;
       tessellationShader                      :VkBool32;
       sampleRateShading                       :VkBool32;
       dualSrcBlend                            :VkBool32;
       logicOp                                 :VkBool32;
       multiDrawIndirect                       :VkBool32;
       drawIndirectFirstInstance               :VkBool32;
       depthClamp                              :VkBool32;
       depthBiasClamp                          :VkBool32;
       fillModeNonSolid                        :VkBool32;
       depthBounds                             :VkBool32;
       wideLines                               :VkBool32;
       largePoints                             :VkBool32;
       alphaToOne                              :VkBool32;
       multiViewport                           :VkBool32;
       samplerAnisotropy                       :VkBool32;
       textureCompressionETC2                  :VkBool32;
       textureCompressionASTC_LDR              :VkBool32;
       textureCompressionBC                    :VkBool32;
       occlusionQueryPrecise                   :VkBool32;
       pipelineStatisticsQuery                 :VkBool32;
       vertexPipelineStoresAndAtomics          :VkBool32;
       fragmentStoresAndAtomics                :VkBool32;
       shaderTessellationAndGeometryPointSize  :VkBool32;
       shaderImageGatherExtended               :VkBool32;
       shaderStorageImageExtendedFormats       :VkBool32;
       shaderStorageImageMultisample           :VkBool32;
       shaderStorageImageReadWithoutFormat     :VkBool32;
       shaderStorageImageWriteWithoutFormat    :VkBool32;
       shaderUniformBufferArrayDynamicIndexing :VkBool32;
       shaderSampledImageArrayDynamicIndexing  :VkBool32;
       shaderStorageBufferArrayDynamicIndexing :VkBool32;
       shaderStorageImageArrayDynamicIndexing  :VkBool32;
       shaderClipDistance                      :VkBool32;
       shaderCullDistance                      :VkBool32;
       shaderFloat64                           :VkBool32;
       shaderInt64                             :VkBool32;
       shaderInt16                             :VkBool32;
       shaderResourceResidency                 :VkBool32;
       shaderResourceMinLod                    :VkBool32;
       sparseBinding                           :VkBool32;
       sparseResidencyBuffer                   :VkBool32;
       sparseResidencyImage2D                  :VkBool32;
       sparseResidencyImage3D                  :VkBool32;
       sparseResidency2Samples                 :VkBool32;
       sparseResidency4Samples                 :VkBool32;
       sparseResidency8Samples                 :VkBool32;
       sparseResidency16Samples                :VkBool32;
       sparseResidencyAliased                  :VkBool32;
       variableMultisampleRate                 :VkBool32;
       inheritedQueries                        :VkBool32;
     end;

type P_VkPhysicalDeviceLimits = ^VkPhysicalDeviceLimits;
     VkPhysicalDeviceLimits = record
       maxImageDimension1D                             :T_uint32_t;
       maxImageDimension2D                             :T_uint32_t;
       maxImageDimension3D                             :T_uint32_t;
       maxImageDimensionCube                           :T_uint32_t;
       maxImageArrayLayers                             :T_uint32_t;
       maxTexelBufferElements                          :T_uint32_t;
       maxUniformBufferRange                           :T_uint32_t;
       maxStorageBufferRange                           :T_uint32_t;
       maxPushConstantsSize                            :T_uint32_t;
       maxMemoryAllocationCount                        :T_uint32_t;
       maxSamplerAllocationCount                       :T_uint32_t;
       bufferImageGranularity                          :VkDeviceSize;
       sparseAddressSpaceSize                          :VkDeviceSize;
       maxBoundDescriptorSets                          :T_uint32_t;
       maxPerStageDescriptorSamplers                   :T_uint32_t;
       maxPerStageDescriptorUniformBuffers             :T_uint32_t;
       maxPerStageDescriptorStorageBuffers             :T_uint32_t;
       maxPerStageDescriptorSampledImages              :T_uint32_t;
       maxPerStageDescriptorStorageImages              :T_uint32_t;
       maxPerStageDescriptorInputAttachments           :T_uint32_t;
       maxPerStageResources                            :T_uint32_t;
       maxDescriptorSetSamplers                        :T_uint32_t;
       maxDescriptorSetUniformBuffers                  :T_uint32_t;
       maxDescriptorSetUniformBuffersDynamic           :T_uint32_t;
       maxDescriptorSetStorageBuffers                  :T_uint32_t;
       maxDescriptorSetStorageBuffersDynamic           :T_uint32_t;
       maxDescriptorSetSampledImages                   :T_uint32_t;
       maxDescriptorSetStorageImages                   :T_uint32_t;
       maxDescriptorSetInputAttachments                :T_uint32_t;
       maxVertexInputAttributes                        :T_uint32_t;
       maxVertexInputBindings                          :T_uint32_t;
       maxVertexInputAttributeOffset                   :T_uint32_t;
       maxVertexInputBindingStride                     :T_uint32_t;
       maxVertexOutputComponents                       :T_uint32_t;
       maxTessellationGenerationLevel                  :T_uint32_t;
       maxTessellationPatchSize                        :T_uint32_t;
       maxTessellationControlPerVertexInputComponents  :T_uint32_t;
       maxTessellationControlPerVertexOutputComponents :T_uint32_t;
       maxTessellationControlPerPatchOutputComponents  :T_uint32_t;
       maxTessellationControlTotalOutputComponents     :T_uint32_t;
       maxTessellationEvaluationInputComponents        :T_uint32_t;
       maxTessellationEvaluationOutputComponents       :T_uint32_t;
       maxGeometryShaderInvocations                    :T_uint32_t;
       maxGeometryInputComponents                      :T_uint32_t;
       maxGeometryOutputComponents                     :T_uint32_t;
       maxGeometryOutputVertices                       :T_uint32_t;
       maxGeometryTotalOutputComponents                :T_uint32_t;
       maxFragmentInputComponents                      :T_uint32_t;
       maxFragmentOutputAttachments                    :T_uint32_t;
       maxFragmentDualSrcAttachments                   :T_uint32_t;
       maxFragmentCombinedOutputResources              :T_uint32_t;
       maxComputeSharedMemorySize                      :T_uint32_t;
       maxComputeWorkGroupCount                        :array [ 0..3-1 ] of T_uint32_t;
       maxComputeWorkGroupInvocations                  :T_uint32_t;
       maxComputeWorkGroupSize                         :array [ 0..3-1 ] of T_uint32_t;
       subPixelPrecisionBits                           :T_uint32_t;
       subTexelPrecisionBits                           :T_uint32_t;
       mipmapPrecisionBits                             :T_uint32_t;
       maxDrawIndexedIndexValue                        :T_uint32_t;
       maxDrawIndirectCount                            :T_uint32_t;
       maxSamplerLodBias                               :T_float;
       maxSamplerAnisotropy                            :T_float;
       maxViewports                                    :T_uint32_t;
       maxViewportDimensions                           :array [ 0..2-1 ] of T_uint32_t;
       viewportBoundsRange                             :array [ 0..2-1 ] of T_float;
       viewportSubPixelBits                            :T_uint32_t;
       minMemoryMapAlignment                           :T_size_t;
       minTexelBufferOffsetAlignment                   :VkDeviceSize;
       minUniformBufferOffsetAlignment                 :VkDeviceSize;
       minStorageBufferOffsetAlignment                 :VkDeviceSize;
       minTexelOffset                                  :T_int32_t;
       maxTexelOffset                                  :T_uint32_t;
       minTexelGatherOffset                            :T_int32_t;
       maxTexelGatherOffset                            :T_uint32_t;
       minInterpolationOffset                          :T_float;
       maxInterpolationOffset                          :T_float;
       subPixelInterpolationOffsetBits                 :T_uint32_t;
       maxFramebufferWidth                             :T_uint32_t;
       maxFramebufferHeight                            :T_uint32_t;
       maxFramebufferLayers                            :T_uint32_t;
       framebufferColorSampleCounts                    :VkSampleCountFlags;
       framebufferDepthSampleCounts                    :VkSampleCountFlags;
       framebufferStencilSampleCounts                  :VkSampleCountFlags;
       framebufferNoAttachmentsSampleCounts            :VkSampleCountFlags;
       maxColorAttachments                             :T_uint32_t;
       sampledImageColorSampleCounts                   :VkSampleCountFlags;
       sampledImageIntegerSampleCounts                 :VkSampleCountFlags;
       sampledImageDepthSampleCounts                   :VkSampleCountFlags;
       sampledImageStencilSampleCounts                 :VkSampleCountFlags;
       storageImageSampleCounts                        :VkSampleCountFlags;
       maxSampleMaskWords                              :T_uint32_t;
       timestampComputeAndGraphics                     :VkBool32;
       timestampPeriod                                 :T_float;
       maxClipDistances                                :T_uint32_t;
       maxCullDistances                                :T_uint32_t;
       maxCombinedClipAndCullDistances                 :T_uint32_t;
       discreteQueuePriorities                         :T_uint32_t;
       pointSizeRange                                  :array [ 0..2-1 ] of T_float;
       lineWidthRange                                  :array [ 0..2-1 ] of T_float;
       pointSizeGranularity                            :T_float;
       lineWidthGranularity                            :T_float;
       strictLines                                     :VkBool32;
       standardSampleLocations                         :VkBool32;
       optimalBufferCopyOffsetAlignment                :VkDeviceSize;
       optimalBufferCopyRowPitchAlignment              :VkDeviceSize;
       nonCoherentAtomSize                             :VkDeviceSize;
     end;

type P_VkPhysicalDeviceMemoryProperties = ^VkPhysicalDeviceMemoryProperties;
     VkPhysicalDeviceMemoryProperties = record
       memoryTypeCount :T_uint32_t;
       memoryTypes     :array [ 0..VK_MAX_MEMORY_TYPES-1 ] of VkMemoryType;
       memoryHeapCount :T_uint32_t;
       memoryHeaps     :array [ 0..VK_MAX_MEMORY_HEAPS-1 ] of VkMemoryHeap;
     end;

type P_VkPhysicalDeviceSparseProperties = ^VkPhysicalDeviceSparseProperties;
     VkPhysicalDeviceSparseProperties = record
       residencyStandard2DBlockShape            :VkBool32;
       residencyStandard2DMultisampleBlockShape :VkBool32;
       residencyStandard3DBlockShape            :VkBool32;
       residencyAlignedMipSize                  :VkBool32;
       residencyNonResidentStrict               :VkBool32;
     end;

type P_VkPhysicalDeviceProperties = ^VkPhysicalDeviceProperties;
     VkPhysicalDeviceProperties = record
       apiVersion        :T_uint32_t;
       driverVersion     :T_uint32_t;
       vendorID          :T_uint32_t;
       deviceID          :T_uint32_t;
       deviceType        :VkPhysicalDeviceType;
       deviceName        :array [ 0..VK_MAX_PHYSICAL_DEVICE_NAME_SIZE-1 ] of T_char;
       pipelineCacheUUID :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
       limits            :VkPhysicalDeviceLimits;
       sparseProperties  :VkPhysicalDeviceSparseProperties;
     end;

type P_VkQueueFamilyProperties = ^VkQueueFamilyProperties;
     VkQueueFamilyProperties = record
       queueFlags                  :VkQueueFlags;
       queueCount                  :T_uint32_t;
       timestampValidBits          :T_uint32_t;
       minImageTransferGranularity :VkExtent3D;
     end;

type P_VkDeviceQueueCreateInfo = ^VkDeviceQueueCreateInfo;
     VkDeviceQueueCreateInfo = record
       sType            :VkStructureType;
       pNext            :P_void;
       flags            :VkDeviceQueueCreateFlags;
       queueFamilyIndex :T_uint32_t;
       queueCount       :T_uint32_t;
       pQueuePriorities :P_float;
     end;

type P_VkDeviceCreateInfo = ^VkDeviceCreateInfo;
     VkDeviceCreateInfo = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       flags                   :VkDeviceCreateFlags;
       queueCreateInfoCount    :T_uint32_t;
       pQueueCreateInfos       :P_VkDeviceQueueCreateInfo;
       enabledLayerCount       :T_uint32_t;
       ppEnabledLayerNames     :PP_char;
       enabledExtensionCount   :T_uint32_t;
       ppEnabledExtensionNames :PP_char;
       pEnabledFeatures        :P_VkPhysicalDeviceFeatures;
     end;

type P_VkExtensionProperties = ^VkExtensionProperties;
     VkExtensionProperties = record
       extensionName :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
       specVersion   :T_uint32_t;
     end;

type P_VkLayerProperties = ^VkLayerProperties;
     VkLayerProperties = record
       layerName             :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
       specVersion           :T_uint32_t;
       implementationVersion :T_uint32_t;
       description           :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
     end;

type P_VkSubmitInfo = ^VkSubmitInfo;
     VkSubmitInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       waitSemaphoreCount   :T_uint32_t;
       pWaitSemaphores      :P_VkSemaphore;
       pWaitDstStageMask    :P_VkPipelineStageFlags;
       commandBufferCount   :T_uint32_t;
       pCommandBuffers      :P_VkCommandBuffer;
       signalSemaphoreCount :T_uint32_t;
       pSignalSemaphores    :P_VkSemaphore;
     end;

type P_VkMappedMemoryRange = ^VkMappedMemoryRange;
     VkMappedMemoryRange = record
       sType  :VkStructureType;
       pNext  :P_void;
       memory :VkDeviceMemory;
       offset :VkDeviceSize;
       size   :VkDeviceSize;
     end;

type P_VkMemoryAllocateInfo = ^VkMemoryAllocateInfo;
     VkMemoryAllocateInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       allocationSize  :VkDeviceSize;
       memoryTypeIndex :T_uint32_t;
     end;

type P_VkMemoryRequirements = ^VkMemoryRequirements;
     VkMemoryRequirements = record
       size           :VkDeviceSize;
       alignment      :VkDeviceSize;
       memoryTypeBits :T_uint32_t;
     end;

type P_VkSparseMemoryBind = ^VkSparseMemoryBind;
     VkSparseMemoryBind = record
       resourceOffset :VkDeviceSize;
       size           :VkDeviceSize;
       memory         :VkDeviceMemory;
       memoryOffset   :VkDeviceSize;
       flags          :VkSparseMemoryBindFlags;
     end;

type P_VkSparseBufferMemoryBindInfo = ^VkSparseBufferMemoryBindInfo;
     VkSparseBufferMemoryBindInfo = record
       buffer    :VkBuffer;
       bindCount :T_uint32_t;
       pBinds    :P_VkSparseMemoryBind;
     end;

type P_VkSparseImageOpaqueMemoryBindInfo = ^VkSparseImageOpaqueMemoryBindInfo;
     VkSparseImageOpaqueMemoryBindInfo = record
       image     :VkImage;
       bindCount :T_uint32_t;
       pBinds    :P_VkSparseMemoryBind;
     end;

type P_VkImageSubresource = ^VkImageSubresource;
     VkImageSubresource = record
       aspectMask :VkImageAspectFlags;
       mipLevel   :T_uint32_t;
       arrayLayer :T_uint32_t;
     end;

type P_VkSparseImageMemoryBind = ^VkSparseImageMemoryBind;
     VkSparseImageMemoryBind = record
       subresource  :VkImageSubresource;
       offset       :VkOffset3D;
       extent       :VkExtent3D;
       memory       :VkDeviceMemory;
       memoryOffset :VkDeviceSize;
       flags        :VkSparseMemoryBindFlags;
     end;

type P_VkSparseImageMemoryBindInfo = ^VkSparseImageMemoryBindInfo;
     VkSparseImageMemoryBindInfo = record
       image     :VkImage;
       bindCount :T_uint32_t;
       pBinds    :P_VkSparseImageMemoryBind;
     end;

type P_VkBindSparseInfo = ^VkBindSparseInfo;
     VkBindSparseInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       waitSemaphoreCount   :T_uint32_t;
       pWaitSemaphores      :P_VkSemaphore;
       bufferBindCount      :T_uint32_t;
       pBufferBinds         :P_VkSparseBufferMemoryBindInfo;
       imageOpaqueBindCount :T_uint32_t;
       pImageOpaqueBinds    :P_VkSparseImageOpaqueMemoryBindInfo;
       imageBindCount       :T_uint32_t;
       pImageBinds          :P_VkSparseImageMemoryBindInfo;
       signalSemaphoreCount :T_uint32_t;
       pSignalSemaphores    :P_VkSemaphore;
     end;

type P_VkSparseImageFormatProperties = ^VkSparseImageFormatProperties;
     VkSparseImageFormatProperties = record
       aspectMask       :VkImageAspectFlags;
       imageGranularity :VkExtent3D;
       flags            :VkSparseImageFormatFlags;
     end;

type P_VkSparseImageMemoryRequirements = ^VkSparseImageMemoryRequirements;
     VkSparseImageMemoryRequirements = record
       formatProperties     :VkSparseImageFormatProperties;
       imageMipTailFirstLod :T_uint32_t;
       imageMipTailSize     :VkDeviceSize;
       imageMipTailOffset   :VkDeviceSize;
       imageMipTailStride   :VkDeviceSize;
     end;

type P_VkFenceCreateInfo = ^VkFenceCreateInfo;
     VkFenceCreateInfo = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkFenceCreateFlags;
     end;

type P_VkSemaphoreCreateInfo = ^VkSemaphoreCreateInfo;
     VkSemaphoreCreateInfo = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkSemaphoreCreateFlags;
     end;

type P_VkEventCreateInfo = ^VkEventCreateInfo;
     VkEventCreateInfo = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkEventCreateFlags;
     end;

type P_VkQueryPoolCreateInfo = ^VkQueryPoolCreateInfo;
     VkQueryPoolCreateInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       flags              :VkQueryPoolCreateFlags;
       queryType          :VkQueryType;
       queryCount         :T_uint32_t;
       pipelineStatistics :VkQueryPipelineStatisticFlags;
     end;

type P_VkBufferCreateInfo = ^VkBufferCreateInfo;
     VkBufferCreateInfo = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       flags                 :VkBufferCreateFlags;
       size                  :VkDeviceSize;
       usage                 :VkBufferUsageFlags;
       sharingMode           :VkSharingMode;
       queueFamilyIndexCount :T_uint32_t;
       pQueueFamilyIndices   :P_uint32_t;
     end;

type P_VkBufferViewCreateInfo = ^VkBufferViewCreateInfo;
     VkBufferViewCreateInfo = record
       sType  :VkStructureType;
       pNext  :P_void;
       flags  :VkBufferViewCreateFlags;
       buffer :VkBuffer;
       format :VkFormat;
       offset :VkDeviceSize;
       range  :VkDeviceSize;
     end;

type P_VkImageCreateInfo = ^VkImageCreateInfo;
     VkImageCreateInfo = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       flags                 :VkImageCreateFlags;
       imageType             :VkImageType;
       format                :VkFormat;
       extent                :VkExtent3D;
       mipLevels             :T_uint32_t;
       arrayLayers           :T_uint32_t;
       samples               :VkSampleCountFlagBits;
       tiling                :VkImageTiling;
       usage                 :VkImageUsageFlags;
       sharingMode           :VkSharingMode;
       queueFamilyIndexCount :T_uint32_t;
       pQueueFamilyIndices   :P_uint32_t;
       initialLayout         :VkImageLayout;
     end;

type P_VkSubresourceLayout = ^VkSubresourceLayout;
     VkSubresourceLayout = record
       offset     :VkDeviceSize;
       size       :VkDeviceSize;
       rowPitch   :VkDeviceSize;
       arrayPitch :VkDeviceSize;
       depthPitch :VkDeviceSize;
     end;

type P_VkComponentMapping = ^VkComponentMapping;
     VkComponentMapping = record
       r :VkComponentSwizzle;
       g :VkComponentSwizzle;
       b :VkComponentSwizzle;
       a :VkComponentSwizzle;
     end;

type P_VkImageViewCreateInfo = ^VkImageViewCreateInfo;
     VkImageViewCreateInfo = record
       sType            :VkStructureType;
       pNext            :P_void;
       flags            :VkImageViewCreateFlags;
       image            :VkImage;
       viewType         :VkImageViewType;
       format           :VkFormat;
       components       :VkComponentMapping;
       subresourceRange :VkImageSubresourceRange;
     end;

type P_VkShaderModuleCreateInfo = ^VkShaderModuleCreateInfo;
     VkShaderModuleCreateInfo = record
       sType    :VkStructureType;
       pNext    :P_void;
       flags    :VkShaderModuleCreateFlags;
       codeSize :T_size_t;
       pCode    :P_uint32_t;
     end;

type P_VkPipelineCacheCreateInfo = ^VkPipelineCacheCreateInfo;
     VkPipelineCacheCreateInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       flags           :VkPipelineCacheCreateFlags;
       initialDataSize :T_size_t;
       pInitialData    :P_void;
     end;

type P_VkSpecializationMapEntry = ^VkSpecializationMapEntry;
     VkSpecializationMapEntry = record
       constantID :T_uint32_t;
       offset     :T_uint32_t;
       size       :T_size_t;
     end;

type P_VkSpecializationInfo = ^VkSpecializationInfo;
     VkSpecializationInfo = record
       mapEntryCount :T_uint32_t;
       pMapEntries   :P_VkSpecializationMapEntry;
       dataSize      :T_size_t;
       pData         :P_void;
     end;

type P_VkPipelineShaderStageCreateInfo = ^VkPipelineShaderStageCreateInfo;
     VkPipelineShaderStageCreateInfo = record
       sType               :VkStructureType;
       pNext               :P_void;
       flags               :VkPipelineShaderStageCreateFlags;
       stage               :VkShaderStageFlagBits;
       module              :VkShaderModule;
       pName               :P_char;
       pSpecializationInfo :P_VkSpecializationInfo;
     end;

type P_VkComputePipelineCreateInfo = ^VkComputePipelineCreateInfo;
     VkComputePipelineCreateInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       flags              :VkPipelineCreateFlags;
       stage              :VkPipelineShaderStageCreateInfo;
       layout             :VkPipelineLayout;
       basePipelineHandle :VkPipeline;
       basePipelineIndex  :T_int32_t;
     end;

type P_VkVertexInputBindingDescription = ^VkVertexInputBindingDescription;
     VkVertexInputBindingDescription = record
       binding   :T_uint32_t;
       stride    :T_uint32_t;
       inputRate :VkVertexInputRate;
     end;

type P_VkVertexInputAttributeDescription = ^VkVertexInputAttributeDescription;
     VkVertexInputAttributeDescription = record
       location :T_uint32_t;
       binding  :T_uint32_t;
       format   :VkFormat;
       offset   :T_uint32_t;
     end;

type P_VkPipelineVertexInputStateCreateInfo = ^VkPipelineVertexInputStateCreateInfo;
     VkPipelineVertexInputStateCreateInfo = record
       sType                           :VkStructureType;
       pNext                           :P_void;
       flags                           :VkPipelineVertexInputStateCreateFlags;
       vertexBindingDescriptionCount   :T_uint32_t;
       pVertexBindingDescriptions      :P_VkVertexInputBindingDescription;
       vertexAttributeDescriptionCount :T_uint32_t;
       pVertexAttributeDescriptions    :P_VkVertexInputAttributeDescription;
     end;

type P_VkPipelineInputAssemblyStateCreateInfo = ^VkPipelineInputAssemblyStateCreateInfo;
     VkPipelineInputAssemblyStateCreateInfo = record
       sType                  :VkStructureType;
       pNext                  :P_void;
       flags                  :VkPipelineInputAssemblyStateCreateFlags;
       topology               :VkPrimitiveTopology;
       primitiveRestartEnable :VkBool32;
     end;

type P_VkPipelineTessellationStateCreateInfo = ^VkPipelineTessellationStateCreateInfo;
     VkPipelineTessellationStateCreateInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       flags              :VkPipelineTessellationStateCreateFlags;
       patchControlPoints :T_uint32_t;
     end;

type P_VkViewport = ^VkViewport;
     VkViewport = record
       x        :T_float;
       y        :T_float;
       width    :T_float;
       height   :T_float;
       minDepth :T_float;
       maxDepth :T_float;
     end;

type P_VkPipelineViewportStateCreateInfo = ^VkPipelineViewportStateCreateInfo;
     VkPipelineViewportStateCreateInfo = record
       sType         :VkStructureType;
       pNext         :P_void;
       flags         :VkPipelineViewportStateCreateFlags;
       viewportCount :T_uint32_t;
       pViewports    :P_VkViewport;
       scissorCount  :T_uint32_t;
       pScissors     :P_VkRect2D;
     end;

type P_VkPipelineRasterizationStateCreateInfo = ^VkPipelineRasterizationStateCreateInfo;
     VkPipelineRasterizationStateCreateInfo = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       flags                   :VkPipelineRasterizationStateCreateFlags;
       depthClampEnable        :VkBool32;
       rasterizerDiscardEnable :VkBool32;
       polygonMode             :VkPolygonMode;
       cullMode                :VkCullModeFlags;
       frontFace               :VkFrontFace;
       depthBiasEnable         :VkBool32;
       depthBiasConstantFactor :T_float;
       depthBiasClamp          :T_float;
       depthBiasSlopeFactor    :T_float;
       lineWidth               :T_float;
     end;

type P_VkPipelineMultisampleStateCreateInfo = ^VkPipelineMultisampleStateCreateInfo;
     VkPipelineMultisampleStateCreateInfo = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       flags                 :VkPipelineMultisampleStateCreateFlags;
       rasterizationSamples  :VkSampleCountFlagBits;
       sampleShadingEnable   :VkBool32;
       minSampleShading      :T_float;
       pSampleMask           :P_VkSampleMask;
       alphaToCoverageEnable :VkBool32;
       alphaToOneEnable      :VkBool32;
     end;

type P_VkStencilOpState = ^VkStencilOpState;
     VkStencilOpState = record
       failOp      :VkStencilOp;
       passOp      :VkStencilOp;
       depthFailOp :VkStencilOp;
       compareOp   :VkCompareOp;
       compareMask :T_uint32_t;
       writeMask   :T_uint32_t;
       reference   :T_uint32_t;
     end;

type P_VkPipelineDepthStencilStateCreateInfo = ^VkPipelineDepthStencilStateCreateInfo;
     VkPipelineDepthStencilStateCreateInfo = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       flags                 :VkPipelineDepthStencilStateCreateFlags;
       depthTestEnable       :VkBool32;
       depthWriteEnable      :VkBool32;
       depthCompareOp        :VkCompareOp;
       depthBoundsTestEnable :VkBool32;
       stencilTestEnable     :VkBool32;
       front                 :VkStencilOpState;
       back                  :VkStencilOpState;
       minDepthBounds        :T_float;
       maxDepthBounds        :T_float;
     end;

type P_VkPipelineColorBlendAttachmentState = ^VkPipelineColorBlendAttachmentState;
     VkPipelineColorBlendAttachmentState = record
       blendEnable         :VkBool32;
       srcColorBlendFactor :VkBlendFactor;
       dstColorBlendFactor :VkBlendFactor;
       colorBlendOp        :VkBlendOp;
       srcAlphaBlendFactor :VkBlendFactor;
       dstAlphaBlendFactor :VkBlendFactor;
       alphaBlendOp        :VkBlendOp;
       colorWriteMask      :VkColorComponentFlags;
     end;

type P_VkPipelineColorBlendStateCreateInfo = ^VkPipelineColorBlendStateCreateInfo;
     VkPipelineColorBlendStateCreateInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       flags           :VkPipelineColorBlendStateCreateFlags;
       logicOpEnable   :VkBool32;
       logicOp         :VkLogicOp;
       attachmentCount :T_uint32_t;
       pAttachments    :P_VkPipelineColorBlendAttachmentState;
       blendConstants  :array [ 0..4-1 ] of T_float;
     end;

type P_VkPipelineDynamicStateCreateInfo = ^VkPipelineDynamicStateCreateInfo;
     VkPipelineDynamicStateCreateInfo = record
       sType             :VkStructureType;
       pNext             :P_void;
       flags             :VkPipelineDynamicStateCreateFlags;
       dynamicStateCount :T_uint32_t;
       pDynamicStates    :P_VkDynamicState;
     end;

type P_VkGraphicsPipelineCreateInfo = ^VkGraphicsPipelineCreateInfo;
     VkGraphicsPipelineCreateInfo = record
       sType               :VkStructureType;
       pNext               :P_void;
       flags               :VkPipelineCreateFlags;
       stageCount          :T_uint32_t;
       pStages             :P_VkPipelineShaderStageCreateInfo;
       pVertexInputState   :P_VkPipelineVertexInputStateCreateInfo;
       pInputAssemblyState :P_VkPipelineInputAssemblyStateCreateInfo;
       pTessellationState  :P_VkPipelineTessellationStateCreateInfo;
       pViewportState      :P_VkPipelineViewportStateCreateInfo;
       pRasterizationState :P_VkPipelineRasterizationStateCreateInfo;
       pMultisampleState   :P_VkPipelineMultisampleStateCreateInfo;
       pDepthStencilState  :P_VkPipelineDepthStencilStateCreateInfo;
       pColorBlendState    :P_VkPipelineColorBlendStateCreateInfo;
       pDynamicState       :P_VkPipelineDynamicStateCreateInfo;
       layout              :VkPipelineLayout;
       renderPass          :VkRenderPass;
       subpass             :T_uint32_t;
       basePipelineHandle  :VkPipeline;
       basePipelineIndex   :T_int32_t;
     end;

type P_VkPushConstantRange = ^VkPushConstantRange;
     VkPushConstantRange = record
       stageFlags :VkShaderStageFlags;
       offset     :T_uint32_t;
       size       :T_uint32_t;
     end;

type P_VkPipelineLayoutCreateInfo = ^VkPipelineLayoutCreateInfo;
     VkPipelineLayoutCreateInfo = record
       sType                  :VkStructureType;
       pNext                  :P_void;
       flags                  :VkPipelineLayoutCreateFlags;
       setLayoutCount         :T_uint32_t;
       pSetLayouts            :P_VkDescriptorSetLayout;
       pushConstantRangeCount :T_uint32_t;
       pPushConstantRanges    :P_VkPushConstantRange;
     end;

type P_VkSamplerCreateInfo = ^VkSamplerCreateInfo;
     VkSamplerCreateInfo = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       flags                   :VkSamplerCreateFlags;
       magFilter               :VkFilter;
       minFilter               :VkFilter;
       mipmapMode              :VkSamplerMipmapMode;
       addressModeU            :VkSamplerAddressMode;
       addressModeV            :VkSamplerAddressMode;
       addressModeW            :VkSamplerAddressMode;
       mipLodBias              :T_float;
       anisotropyEnable        :VkBool32;
       maxAnisotropy           :T_float;
       compareEnable           :VkBool32;
       compareOp               :VkCompareOp;
       minLod                  :T_float;
       maxLod                  :T_float;
       borderColor             :VkBorderColor;
       unnormalizedCoordinates :VkBool32;
     end;

type P_VkCopyDescriptorSet = ^VkCopyDescriptorSet;
     VkCopyDescriptorSet = record
       sType           :VkStructureType;
       pNext           :P_void;
       srcSet          :VkDescriptorSet;
       srcBinding      :T_uint32_t;
       srcArrayElement :T_uint32_t;
       dstSet          :VkDescriptorSet;
       dstBinding      :T_uint32_t;
       dstArrayElement :T_uint32_t;
       descriptorCount :T_uint32_t;
     end;

type P_VkDescriptorBufferInfo = ^VkDescriptorBufferInfo;
     VkDescriptorBufferInfo = record
       buffer :VkBuffer;
       offset :VkDeviceSize;
       range  :VkDeviceSize;
     end;

type P_VkDescriptorImageInfo = ^VkDescriptorImageInfo;
     VkDescriptorImageInfo = record
       sampler     :VkSampler;
       imageView   :VkImageView;
       imageLayout :VkImageLayout;
     end;

type P_VkDescriptorPoolSize = ^VkDescriptorPoolSize;
     VkDescriptorPoolSize = record
       type_           :VkDescriptorType;
       descriptorCount :T_uint32_t;
     end;

type P_VkDescriptorPoolCreateInfo = ^VkDescriptorPoolCreateInfo;
     VkDescriptorPoolCreateInfo = record
       sType         :VkStructureType;
       pNext         :P_void;
       flags         :VkDescriptorPoolCreateFlags;
       maxSets       :T_uint32_t;
       poolSizeCount :T_uint32_t;
       pPoolSizes    :P_VkDescriptorPoolSize;
     end;

type P_VkDescriptorSetAllocateInfo = ^VkDescriptorSetAllocateInfo;
     VkDescriptorSetAllocateInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       descriptorPool     :VkDescriptorPool;
       descriptorSetCount :T_uint32_t;
       pSetLayouts        :P_VkDescriptorSetLayout;
     end;

type P_VkDescriptorSetLayoutBinding = ^VkDescriptorSetLayoutBinding;
     VkDescriptorSetLayoutBinding = record
       binding            :T_uint32_t;
       descriptorType     :VkDescriptorType;
       descriptorCount    :T_uint32_t;
       stageFlags         :VkShaderStageFlags;
       pImmutableSamplers :P_VkSampler;
     end;

type P_VkDescriptorSetLayoutCreateInfo = ^VkDescriptorSetLayoutCreateInfo;
     VkDescriptorSetLayoutCreateInfo = record
       sType        :VkStructureType;
       pNext        :P_void;
       flags        :VkDescriptorSetLayoutCreateFlags;
       bindingCount :T_uint32_t;
       pBindings    :P_VkDescriptorSetLayoutBinding;
     end;

type P_VkWriteDescriptorSet = ^VkWriteDescriptorSet;
     VkWriteDescriptorSet = record
       sType            :VkStructureType;
       pNext            :P_void;
       dstSet           :VkDescriptorSet;
       dstBinding       :T_uint32_t;
       dstArrayElement  :T_uint32_t;
       descriptorCount  :T_uint32_t;
       descriptorType   :VkDescriptorType;
       pImageInfo       :P_VkDescriptorImageInfo;
       pBufferInfo      :P_VkDescriptorBufferInfo;
       pTexelBufferView :P_VkBufferView;
     end;

type P_VkAttachmentDescription = ^VkAttachmentDescription;
     VkAttachmentDescription = record
       flags          :VkAttachmentDescriptionFlags;
       format         :VkFormat;
       samples        :VkSampleCountFlagBits;
       loadOp         :VkAttachmentLoadOp;
       storeOp        :VkAttachmentStoreOp;
       stencilLoadOp  :VkAttachmentLoadOp;
       stencilStoreOp :VkAttachmentStoreOp;
       initialLayout  :VkImageLayout;
       finalLayout    :VkImageLayout;
     end;

type P_VkAttachmentReference = ^VkAttachmentReference;
     VkAttachmentReference = record
       attachment :T_uint32_t;
       layout     :VkImageLayout;
     end;

type P_VkFramebufferCreateInfo = ^VkFramebufferCreateInfo;
     VkFramebufferCreateInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       flags           :VkFramebufferCreateFlags;
       renderPass      :VkRenderPass;
       attachmentCount :T_uint32_t;
       pAttachments    :P_VkImageView;
       width           :T_uint32_t;
       height          :T_uint32_t;
       layers          :T_uint32_t;
     end;

type P_VkSubpassDescription = ^VkSubpassDescription;
     VkSubpassDescription = record
       flags                   :VkSubpassDescriptionFlags;
       pipelineBindPoint       :VkPipelineBindPoint;
       inputAttachmentCount    :T_uint32_t;
       pInputAttachments       :P_VkAttachmentReference;
       colorAttachmentCount    :T_uint32_t;
       pColorAttachments       :P_VkAttachmentReference;
       pResolveAttachments     :P_VkAttachmentReference;
       pDepthStencilAttachment :P_VkAttachmentReference;
       preserveAttachmentCount :T_uint32_t;
       pPreserveAttachments    :P_uint32_t;
     end;

type P_VkSubpassDependency = ^VkSubpassDependency;
     VkSubpassDependency = record
       srcSubpass      :T_uint32_t;
       dstSubpass      :T_uint32_t;
       srcStageMask    :VkPipelineStageFlags;
       dstStageMask    :VkPipelineStageFlags;
       srcAccessMask   :VkAccessFlags;
       dstAccessMask   :VkAccessFlags;
       dependencyFlags :VkDependencyFlags;
     end;

type P_VkRenderPassCreateInfo = ^VkRenderPassCreateInfo;
     VkRenderPassCreateInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       flags           :VkRenderPassCreateFlags;
       attachmentCount :T_uint32_t;
       pAttachments    :P_VkAttachmentDescription;
       subpassCount    :T_uint32_t;
       pSubpasses      :P_VkSubpassDescription;
       dependencyCount :T_uint32_t;
       pDependencies   :P_VkSubpassDependency;
     end;

type P_VkCommandPoolCreateInfo = ^VkCommandPoolCreateInfo;
     VkCommandPoolCreateInfo = record
       sType            :VkStructureType;
       pNext            :P_void;
       flags            :VkCommandPoolCreateFlags;
       queueFamilyIndex :T_uint32_t;
     end;

type P_VkCommandBufferAllocateInfo = ^VkCommandBufferAllocateInfo;
     VkCommandBufferAllocateInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       commandPool        :VkCommandPool;
       level              :VkCommandBufferLevel;
       commandBufferCount :T_uint32_t;
     end;

type P_VkCommandBufferInheritanceInfo = ^VkCommandBufferInheritanceInfo;
     VkCommandBufferInheritanceInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       renderPass           :VkRenderPass;
       subpass              :T_uint32_t;
       framebuffer          :VkFramebuffer;
       occlusionQueryEnable :VkBool32;
       queryFlags           :VkQueryControlFlags;
       pipelineStatistics   :VkQueryPipelineStatisticFlags;
     end;

type P_VkCommandBufferBeginInfo = ^VkCommandBufferBeginInfo;
     VkCommandBufferBeginInfo = record
       sType            :VkStructureType;
       pNext            :P_void;
       flags            :VkCommandBufferUsageFlags;
       pInheritanceInfo :P_VkCommandBufferInheritanceInfo;
     end;

type P_VkBufferCopy = ^VkBufferCopy;
     VkBufferCopy = record
       srcOffset :VkDeviceSize;
       dstOffset :VkDeviceSize;
       size      :VkDeviceSize;
     end;

type P_VkImageSubresourceLayers = ^VkImageSubresourceLayers;
     VkImageSubresourceLayers = record
       aspectMask     :VkImageAspectFlags;
       mipLevel       :T_uint32_t;
       baseArrayLayer :T_uint32_t;
       layerCount     :T_uint32_t;
     end;

type P_VkBufferImageCopy = ^VkBufferImageCopy;
     VkBufferImageCopy = record
       bufferOffset      :VkDeviceSize;
       bufferRowLength   :T_uint32_t;
       bufferImageHeight :T_uint32_t;
       imageSubresource  :VkImageSubresourceLayers;
       imageOffset       :VkOffset3D;
       imageExtent       :VkExtent3D;
     end;

type P_VkClearColorValue = ^VkClearColorValue;
     VkClearColorValue = record
     case Byte of
       0:( float32 :array [ 0..4-1 ] of T_float    );
       1:( int32   :array [ 0..4-1 ] of T_int32_t  );
       2:( uint32  :array [ 0..4-1 ] of T_uint32_t );
     end;

type P_VkClearDepthStencilValue = ^VkClearDepthStencilValue;
     VkClearDepthStencilValue = record
       depth   :T_float;
       stencil :T_uint32_t;
     end;

type P_VkClearValue = ^VkClearValue;
     VkClearValue = record
     case Byte of
       0:( color        :VkClearColorValue        );
       1:( depthStencil :VkClearDepthStencilValue );
     end;

type P_VkClearAttachment = ^VkClearAttachment;
     VkClearAttachment = record
       aspectMask      :VkImageAspectFlags;
       colorAttachment :T_uint32_t;
       clearValue      :VkClearValue;
     end;

type P_VkClearRect = ^VkClearRect;
     VkClearRect = record
       rect           :VkRect2D;
       baseArrayLayer :T_uint32_t;
       layerCount     :T_uint32_t;
     end;

type P_VkImageBlit = ^VkImageBlit;
     VkImageBlit = record
       srcSubresource :VkImageSubresourceLayers;
       srcOffsets     :array [ 0..2-1 ] of VkOffset3D;
       dstSubresource :VkImageSubresourceLayers;
       dstOffsets     :array [ 0..2-1 ] of VkOffset3D;
     end;

type P_VkImageCopy = ^VkImageCopy;
     VkImageCopy = record
       srcSubresource :VkImageSubresourceLayers;
       srcOffset      :VkOffset3D;
       dstSubresource :VkImageSubresourceLayers;
       dstOffset      :VkOffset3D;
       extent         :VkExtent3D;
     end;

type P_VkImageResolve = ^VkImageResolve;
     VkImageResolve = record
       srcSubresource :VkImageSubresourceLayers;
       srcOffset      :VkOffset3D;
       dstSubresource :VkImageSubresourceLayers;
       dstOffset      :VkOffset3D;
       extent         :VkExtent3D;
     end;

type P_VkRenderPassBeginInfo = ^VkRenderPassBeginInfo;
     VkRenderPassBeginInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       renderPass      :VkRenderPass;
       framebuffer     :VkFramebuffer;
       renderArea      :VkRect2D;
       clearValueCount :T_uint32_t;
       pClearValues    :P_VkClearValue;
     end;

type PFN_vkCreateInstance                               = function( const pCreateInfo_:P_VkInstanceCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pInstance_:P_VkInstance ) :VkResult;
type PFN_vkDestroyInstance                              = procedure( instance_:VkInstance; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkEnumeratePhysicalDevices                     = function( instance_:VkInstance; pPhysicalDeviceCount_:P_uint32_t; pPhysicalDevices_:P_VkPhysicalDevice ) :VkResult;
type PFN_vkGetPhysicalDeviceFeatures                    = procedure( physicalDevice_:VkPhysicalDevice; pFeatures_:P_VkPhysicalDeviceFeatures );
type PFN_vkGetPhysicalDeviceFormatProperties            = procedure( physicalDevice_:VkPhysicalDevice; format_:VkFormat; pFormatProperties_:P_VkFormatProperties );
type PFN_vkGetPhysicalDeviceImageFormatProperties       = function( physicalDevice_:VkPhysicalDevice; format_:VkFormat; type_:VkImageType; tiling_:VkImageTiling; usage_:VkImageUsageFlags; flags_:VkImageCreateFlags; pImageFormatProperties_:P_VkImageFormatProperties ) :VkResult;
type PFN_vkGetPhysicalDeviceProperties                  = procedure( physicalDevice_:VkPhysicalDevice; pProperties_:P_VkPhysicalDeviceProperties );
type PFN_vkGetPhysicalDeviceQueueFamilyProperties       = procedure( physicalDevice_:VkPhysicalDevice; pQueueFamilyPropertyCount_:P_uint32_t; pQueueFamilyProperties_:P_VkQueueFamilyProperties );
type PFN_vkGetPhysicalDeviceMemoryProperties            = procedure( physicalDevice_:VkPhysicalDevice; pMemoryProperties_:P_VkPhysicalDeviceMemoryProperties );
type PFN_vkGetInstanceProcAddr                          = function( instance_:VkInstance; const pName_:P_char ) :PFN_vkVoidFunction;
type PFN_vkGetDeviceProcAddr                            = function( device_:VkDevice; const pName_:P_char ) :PFN_vkVoidFunction;
type PFN_vkCreateDevice                                 = function( physicalDevice_:VkPhysicalDevice; const pCreateInfo_:P_VkDeviceCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pDevice_:P_VkDevice ) :VkResult;
type PFN_vkDestroyDevice                                = procedure( device_:VkDevice; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkEnumerateInstanceExtensionProperties         = function( const pLayerName_:P_char; pPropertyCount_:P_uint32_t; pProperties_:P_VkExtensionProperties ) :VkResult;
type PFN_vkEnumerateDeviceExtensionProperties           = function( physicalDevice_:VkPhysicalDevice; const pLayerName_:P_char; pPropertyCount_:P_uint32_t; pProperties_:P_VkExtensionProperties ) :VkResult;
type PFN_vkEnumerateInstanceLayerProperties             = function( pPropertyCount_:P_uint32_t; pProperties_:P_VkLayerProperties ) :VkResult;
type PFN_vkEnumerateDeviceLayerProperties               = function( physicalDevice_:VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkLayerProperties ) :VkResult;
type PFN_vkGetDeviceQueue                               = procedure( device_:VkDevice; queueFamilyIndex_:T_uint32_t; queueIndex_:T_uint32_t; pQueue_:P_VkQueue );
type PFN_vkQueueSubmit                                  = function( queue_:VkQueue; submitCount_:T_uint32_t; const pSubmits_:P_VkSubmitInfo; fence_:VkFence ) :VkResult;
type PFN_vkQueueWaitIdle                                = function( queue_:VkQueue ) :VkResult;
type PFN_vkDeviceWaitIdle                               = function( device_:VkDevice ) :VkResult;
type PFN_vkAllocateMemory                               = function( device_:VkDevice; const pAllocateInfo_:P_VkMemoryAllocateInfo; const pAllocator_:P_VkAllocationCallbacks; pMemory_:P_VkDeviceMemory ) :VkResult;
type PFN_vkFreeMemory                                   = procedure( device_:VkDevice; memory_:VkDeviceMemory; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkMapMemory                                    = function( device_:VkDevice; memory_:VkDeviceMemory; offset_:VkDeviceSize; size_:VkDeviceSize; flags_:VkMemoryMapFlags; ppData_:PP_void ) :VkResult;
type PFN_vkUnmapMemory                                  = procedure( device_:VkDevice; memory_:VkDeviceMemory );
type PFN_vkFlushMappedMemoryRanges                      = function( device_:VkDevice; memoryRangeCount_:T_uint32_t; const pMemoryRanges_:P_VkMappedMemoryRange ) :VkResult;
type PFN_vkInvalidateMappedMemoryRanges                 = function( device_:VkDevice; memoryRangeCount_:T_uint32_t; const pMemoryRanges_:P_VkMappedMemoryRange ) :VkResult;
type PFN_vkGetDeviceMemoryCommitment                    = procedure( device_:VkDevice; memory_:VkDeviceMemory; pCommittedMemoryInBytes_:P_VkDeviceSize );
type PFN_vkBindBufferMemory                             = function( device_:VkDevice; buffer_:VkBuffer; memory_:VkDeviceMemory; memoryOffset_:VkDeviceSize ) :VkResult;
type PFN_vkBindImageMemory                              = function( device_:VkDevice; image_:VkImage; memory_:VkDeviceMemory; memoryOffset_:VkDeviceSize ) :VkResult;
type PFN_vkGetBufferMemoryRequirements                  = procedure( device_:VkDevice; buffer_:VkBuffer; pMemoryRequirements_:P_VkMemoryRequirements );
type PFN_vkGetImageMemoryRequirements                   = procedure( device_:VkDevice; image_:VkImage; pMemoryRequirements_:P_VkMemoryRequirements );
type PFN_vkGetImageSparseMemoryRequirements             = procedure( device_:VkDevice; image_:VkImage; pSparseMemoryRequirementCount_:P_uint32_t; pSparseMemoryRequirements_:P_VkSparseImageMemoryRequirements );
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties = procedure( physicalDevice_:VkPhysicalDevice; format_:VkFormat; type_:VkImageType; samples_:VkSampleCountFlagBits; usage_:VkImageUsageFlags; tiling_:VkImageTiling; pPropertyCount_:P_uint32_t; pProperties_:P_VkSparseImageFormatProperties );
type PFN_vkQueueBindSparse                              = function( queue_:VkQueue; bindInfoCount_:T_uint32_t; const pBindInfo_:P_VkBindSparseInfo; fence_:VkFence ) :VkResult;
type PFN_vkCreateFence                                  = function( device_:VkDevice; const pCreateInfo_:P_VkFenceCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pFence_:P_VkFence ) :VkResult;
type PFN_vkDestroyFence                                 = procedure( device_:VkDevice; fence_:VkFence; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkResetFences                                  = function( device_:VkDevice; fenceCount_:T_uint32_t; const pFences_:P_VkFence ) :VkResult;
type PFN_vkGetFenceStatus                               = function( device_:VkDevice; fence_:VkFence ) :VkResult;
type PFN_vkWaitForFences                                = function( device_:VkDevice; fenceCount_:T_uint32_t; const pFences_:P_VkFence; waitAll_:VkBool32; timeout_:T_uint64_t ) :VkResult;
type PFN_vkCreateSemaphore                              = function( device_:VkDevice; const pCreateInfo_:P_VkSemaphoreCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pSemaphore_:P_VkSemaphore ) :VkResult;
type PFN_vkDestroySemaphore                             = procedure( device_:VkDevice; semaphore_:VkSemaphore; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateEvent                                  = function( device_:VkDevice; const pCreateInfo_:P_VkEventCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pEvent_:P_VkEvent ) :VkResult;
type PFN_vkDestroyEvent                                 = procedure( device_:VkDevice; event_:VkEvent; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetEventStatus                               = function( device_:VkDevice; event_:VkEvent ) :VkResult;
type PFN_vkSetEvent                                     = function( device_:VkDevice; event_:VkEvent ) :VkResult;
type PFN_vkResetEvent                                   = function( device_:VkDevice; event_:VkEvent ) :VkResult;
type PFN_vkCreateQueryPool                              = function( device_:VkDevice; const pCreateInfo_:P_VkQueryPoolCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pQueryPool_:P_VkQueryPool ) :VkResult;
type PFN_vkDestroyQueryPool                             = procedure( device_:VkDevice; queryPool_:VkQueryPool; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetQueryPoolResults                          = function( device_:VkDevice; queryPool_:VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t; dataSize_:T_size_t; pData_:P_void; stride_:VkDeviceSize; flags_:VkQueryResultFlags ) :VkResult;
type PFN_vkCreateBuffer                                 = function( device_:VkDevice; const pCreateInfo_:P_VkBufferCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pBuffer_:P_VkBuffer ) :VkResult;
type PFN_vkDestroyBuffer                                = procedure( device_:VkDevice; buffer_:VkBuffer; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateBufferView                             = function( device_:VkDevice; const pCreateInfo_:P_VkBufferViewCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pView_:P_VkBufferView ) :VkResult;
type PFN_vkDestroyBufferView                            = procedure( device_:VkDevice; bufferView_:VkBufferView; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateImage                                  = function( device_:VkDevice; const pCreateInfo_:P_VkImageCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pImage_:P_VkImage ) :VkResult;
type PFN_vkDestroyImage                                 = procedure( device_:VkDevice; image_:VkImage; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetImageSubresourceLayout                    = procedure( device_:VkDevice; image_:VkImage; const pSubresource_:P_VkImageSubresource; pLayout_:P_VkSubresourceLayout );
type PFN_vkCreateImageView                              = function( device_:VkDevice; const pCreateInfo_:P_VkImageViewCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pView_:P_VkImageView ) :VkResult;
type PFN_vkDestroyImageView                             = procedure( device_:VkDevice; imageView_:VkImageView; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateShaderModule                           = function( device_:VkDevice; const pCreateInfo_:P_VkShaderModuleCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pShaderModule_:P_VkShaderModule ) :VkResult;
type PFN_vkDestroyShaderModule                          = procedure( device_:VkDevice; shaderModule_:VkShaderModule; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreatePipelineCache                          = function( device_:VkDevice; const pCreateInfo_:P_VkPipelineCacheCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelineCache_:P_VkPipelineCache ) :VkResult;
type PFN_vkDestroyPipelineCache                         = procedure( device_:VkDevice; pipelineCache_:VkPipelineCache; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetPipelineCacheData                         = function( device_:VkDevice; pipelineCache_:VkPipelineCache; pDataSize_:P_size_t; pData_:P_void ) :VkResult;
type PFN_vkMergePipelineCaches                          = function( device_:VkDevice; dstCache_:VkPipelineCache; srcCacheCount_:T_uint32_t; const pSrcCaches_:P_VkPipelineCache ) :VkResult;
type PFN_vkCreateGraphicsPipelines                      = function( device_:VkDevice; pipelineCache_:VkPipelineCache; createInfoCount_:T_uint32_t; const pCreateInfos_:P_VkGraphicsPipelineCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelines_:P_VkPipeline ) :VkResult;
type PFN_vkCreateComputePipelines                       = function( device_:VkDevice; pipelineCache_:VkPipelineCache; createInfoCount_:T_uint32_t; const pCreateInfos_:P_VkComputePipelineCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelines_:P_VkPipeline ) :VkResult;
type PFN_vkDestroyPipeline                              = procedure( device_:VkDevice; pipeline_:VkPipeline; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreatePipelineLayout                         = function( device_:VkDevice; const pCreateInfo_:P_VkPipelineLayoutCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelineLayout_:P_VkPipelineLayout ) :VkResult;
type PFN_vkDestroyPipelineLayout                        = procedure( device_:VkDevice; pipelineLayout_:VkPipelineLayout; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateSampler                                = function( device_:VkDevice; const pCreateInfo_:P_VkSamplerCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pSampler_:P_VkSampler ) :VkResult;
type PFN_vkDestroySampler                               = procedure( device_:VkDevice; sampler_:VkSampler; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateDescriptorSetLayout                    = function( device_:VkDevice; const pCreateInfo_:P_VkDescriptorSetLayoutCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pSetLayout_:P_VkDescriptorSetLayout ) :VkResult;
type PFN_vkDestroyDescriptorSetLayout                   = procedure( device_:VkDevice; descriptorSetLayout_:VkDescriptorSetLayout; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateDescriptorPool                         = function( device_:VkDevice; const pCreateInfo_:P_VkDescriptorPoolCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pDescriptorPool_:P_VkDescriptorPool ) :VkResult;
type PFN_vkDestroyDescriptorPool                        = procedure( device_:VkDevice; descriptorPool_:VkDescriptorPool; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkResetDescriptorPool                          = function( device_:VkDevice; descriptorPool_:VkDescriptorPool; flags_:VkDescriptorPoolResetFlags ) :VkResult;
type PFN_vkAllocateDescriptorSets                       = function( device_:VkDevice; const pAllocateInfo_:P_VkDescriptorSetAllocateInfo; pDescriptorSets_:P_VkDescriptorSet ) :VkResult;
type PFN_vkFreeDescriptorSets                           = function( device_:VkDevice; descriptorPool_:VkDescriptorPool; descriptorSetCount_:T_uint32_t; const pDescriptorSets_:P_VkDescriptorSet ) :VkResult;
type PFN_vkUpdateDescriptorSets                         = procedure( device_:VkDevice; descriptorWriteCount_:T_uint32_t; const pDescriptorWrites_:P_VkWriteDescriptorSet; descriptorCopyCount_:T_uint32_t; const pDescriptorCopies_:P_VkCopyDescriptorSet );
type PFN_vkCreateFramebuffer                            = function( device_:VkDevice; const pCreateInfo_:P_VkFramebufferCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pFramebuffer_:P_VkFramebuffer ) :VkResult;
type PFN_vkDestroyFramebuffer                           = procedure( device_:VkDevice; framebuffer_:VkFramebuffer; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateRenderPass                             = function( device_:VkDevice; const pCreateInfo_:P_VkRenderPassCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pRenderPass_:P_VkRenderPass ) :VkResult;
type PFN_vkDestroyRenderPass                            = procedure( device_:VkDevice; renderPass_:VkRenderPass; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetRenderAreaGranularity                     = procedure( device_:VkDevice; renderPass_:VkRenderPass; pGranularity_:P_VkExtent2D );
type PFN_vkCreateCommandPool                            = function( device_:VkDevice; const pCreateInfo_:P_VkCommandPoolCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pCommandPool_:P_VkCommandPool ) :VkResult;
type PFN_vkDestroyCommandPool                           = procedure( device_:VkDevice; commandPool_:VkCommandPool; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkResetCommandPool                             = function( device_:VkDevice; commandPool_:VkCommandPool; flags_:VkCommandPoolResetFlags ) :VkResult;
type PFN_vkAllocateCommandBuffers                       = function( device_:VkDevice; const pAllocateInfo_:P_VkCommandBufferAllocateInfo; pCommandBuffers_:P_VkCommandBuffer ) :VkResult;
type PFN_vkFreeCommandBuffers                           = procedure( device_:VkDevice; commandPool_:VkCommandPool; commandBufferCount_:T_uint32_t; const pCommandBuffers_:P_VkCommandBuffer );
type PFN_vkBeginCommandBuffer                           = function( commandBuffer_:VkCommandBuffer; const pBeginInfo_:P_VkCommandBufferBeginInfo ) :VkResult;
type PFN_vkEndCommandBuffer                             = function( commandBuffer_:VkCommandBuffer ) :VkResult;
type PFN_vkResetCommandBuffer                           = function( commandBuffer_:VkCommandBuffer; flags_:VkCommandBufferResetFlags ) :VkResult;
type PFN_vkCmdBindPipeline                              = procedure( commandBuffer_:VkCommandBuffer; pipelineBindPoint_:VkPipelineBindPoint; pipeline_:VkPipeline );
type PFN_vkCmdSetViewport                               = procedure( commandBuffer_:VkCommandBuffer; firstViewport_:T_uint32_t; viewportCount_:T_uint32_t; const pViewports_:P_VkViewport );
type PFN_vkCmdSetScissor                                = procedure( commandBuffer_:VkCommandBuffer; firstScissor_:T_uint32_t; scissorCount_:T_uint32_t; const pScissors_:P_VkRect2D );
type PFN_vkCmdSetLineWidth                              = procedure( commandBuffer_:VkCommandBuffer; lineWidth_:T_float );
type PFN_vkCmdSetDepthBias                              = procedure( commandBuffer_:VkCommandBuffer; depthBiasConstantFactor_:T_float; depthBiasClamp_:T_float; depthBiasSlopeFactor_:T_float );
type T_blendConstants                                   = array [ 0..4-1 ] of T_float;
     PFN_vkCmdSetBlendConstants                         = procedure( commandBuffer_:VkCommandBuffer; const blendConstants_:T_blendConstants );
type PFN_vkCmdSetDepthBounds                            = procedure( commandBuffer_:VkCommandBuffer; minDepthBounds_:T_float; maxDepthBounds_:T_float );
type PFN_vkCmdSetStencilCompareMask                     = procedure( commandBuffer_:VkCommandBuffer; faceMask_:VkStencilFaceFlags; compareMask_:T_uint32_t );
type PFN_vkCmdSetStencilWriteMask                       = procedure( commandBuffer_:VkCommandBuffer; faceMask_:VkStencilFaceFlags; writeMask_:T_uint32_t );
type PFN_vkCmdSetStencilReference                       = procedure( commandBuffer_:VkCommandBuffer; faceMask_:VkStencilFaceFlags; reference_:T_uint32_t );
type PFN_vkCmdBindDescriptorSets                        = procedure( commandBuffer_:VkCommandBuffer; pipelineBindPoint_:VkPipelineBindPoint; layout_:VkPipelineLayout; firstSet_:T_uint32_t; descriptorSetCount_:T_uint32_t; const pDescriptorSets_:P_VkDescriptorSet; dynamicOffsetCount_:T_uint32_t; const pDynamicOffsets_:P_uint32_t );
type PFN_vkCmdBindIndexBuffer                           = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; indexType_:VkIndexType );
type PFN_vkCmdBindVertexBuffers                         = procedure( commandBuffer_:VkCommandBuffer; firstBinding_:T_uint32_t; bindingCount_:T_uint32_t; const pBuffers_:P_VkBuffer; const pOffsets_:P_VkDeviceSize );
type PFN_vkCmdDraw                                      = procedure( commandBuffer_:VkCommandBuffer; vertexCount_:T_uint32_t; instanceCount_:T_uint32_t; firstVertex_:T_uint32_t; firstInstance_:T_uint32_t );
type PFN_vkCmdDrawIndexed                               = procedure( commandBuffer_:VkCommandBuffer; indexCount_:T_uint32_t; instanceCount_:T_uint32_t; firstIndex_:T_uint32_t; vertexOffset_:T_int32_t; firstInstance_:T_uint32_t );
type PFN_vkCmdDrawIndirect                              = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; drawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCmdDrawIndexedIndirect                       = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; drawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCmdDispatch                                  = procedure( commandBuffer_:VkCommandBuffer; groupCountX_:T_uint32_t; groupCountY_:T_uint32_t; groupCountZ_:T_uint32_t );
type PFN_vkCmdDispatchIndirect                          = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize );
type PFN_vkCmdCopyBuffer                                = procedure( commandBuffer_:VkCommandBuffer; srcBuffer_:VkBuffer; dstBuffer_:VkBuffer; regionCount_:T_uint32_t; const pRegions_:P_VkBufferCopy );
type PFN_vkCmdCopyImage                                 = procedure( commandBuffer_:VkCommandBuffer; srcImage_:VkImage; srcImageLayout_:VkImageLayout; dstImage_:VkImage; dstImageLayout_:VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkImageCopy );
type PFN_vkCmdBlitImage                                 = procedure( commandBuffer_:VkCommandBuffer; srcImage_:VkImage; srcImageLayout_:VkImageLayout; dstImage_:VkImage; dstImageLayout_:VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkImageBlit; filter_:VkFilter );
type PFN_vkCmdCopyBufferToImage                         = procedure( commandBuffer_:VkCommandBuffer; srcBuffer_:VkBuffer; dstImage_:VkImage; dstImageLayout_:VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkBufferImageCopy );
type PFN_vkCmdCopyImageToBuffer                         = procedure( commandBuffer_:VkCommandBuffer; srcImage_:VkImage; srcImageLayout_:VkImageLayout; dstBuffer_:VkBuffer; regionCount_:T_uint32_t; const pRegions_:P_VkBufferImageCopy );
type PFN_vkCmdUpdateBuffer                              = procedure( commandBuffer_:VkCommandBuffer; dstBuffer_:VkBuffer; dstOffset_:VkDeviceSize; dataSize_:VkDeviceSize; const pData_:P_void );
type PFN_vkCmdFillBuffer                                = procedure( commandBuffer_:VkCommandBuffer; dstBuffer_:VkBuffer; dstOffset_:VkDeviceSize; size_:VkDeviceSize; data_:T_uint32_t );
type PFN_vkCmdClearColorImage                           = procedure( commandBuffer_:VkCommandBuffer; image_:VkImage; imageLayout_:VkImageLayout; const pColor_:P_VkClearColorValue; rangeCount_:T_uint32_t; const pRanges_:P_VkImageSubresourceRange );
type PFN_vkCmdClearDepthStencilImage                    = procedure( commandBuffer_:VkCommandBuffer; image_:VkImage; imageLayout_:VkImageLayout; const pDepthStencil_:P_VkClearDepthStencilValue; rangeCount_:T_uint32_t; const pRanges_:P_VkImageSubresourceRange );
type PFN_vkCmdClearAttachments                          = procedure( commandBuffer_:VkCommandBuffer; attachmentCount_:T_uint32_t; const pAttachments_:P_VkClearAttachment; rectCount_:T_uint32_t; const pRects_:P_VkClearRect );
type PFN_vkCmdResolveImage                              = procedure( commandBuffer_:VkCommandBuffer; srcImage_:VkImage; srcImageLayout_:VkImageLayout; dstImage_:VkImage; dstImageLayout_:VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkImageResolve );
type PFN_vkCmdSetEvent                                  = procedure( commandBuffer_:VkCommandBuffer; event_:VkEvent; stageMask_:VkPipelineStageFlags );
type PFN_vkCmdResetEvent                                = procedure( commandBuffer_:VkCommandBuffer; event_:VkEvent; stageMask_:VkPipelineStageFlags );
type PFN_vkCmdWaitEvents                                = procedure( commandBuffer_:VkCommandBuffer; eventCount_:T_uint32_t; const pEvents_:P_VkEvent; srcStageMask_:VkPipelineStageFlags; dstStageMask_:VkPipelineStageFlags; memoryBarrierCount_:T_uint32_t; const pMemoryBarriers_:P_VkMemoryBarrier; bufferMemoryBarrierCount_:T_uint32_t; const pBufferMemoryBarriers_:P_VkBufferMemoryBarrier; imageMemoryBarrierCount_:T_uint32_t; const pImageMemoryBarriers_:P_VkImageMemoryBarrier );
type PFN_vkCmdPipelineBarrier                           = procedure( commandBuffer_:VkCommandBuffer; srcStageMask_:VkPipelineStageFlags; dstStageMask_:VkPipelineStageFlags; dependencyFlags_:VkDependencyFlags; memoryBarrierCount_:T_uint32_t; const pMemoryBarriers_:P_VkMemoryBarrier; bufferMemoryBarrierCount_:T_uint32_t; const pBufferMemoryBarriers_:P_VkBufferMemoryBarrier; imageMemoryBarrierCount_:T_uint32_t; const pImageMemoryBarriers_:P_VkImageMemoryBarrier );
type PFN_vkCmdBeginQuery                                = procedure( commandBuffer_:VkCommandBuffer; queryPool_:VkQueryPool; query_:T_uint32_t; flags_:VkQueryControlFlags );
type PFN_vkCmdEndQuery                                  = procedure( commandBuffer_:VkCommandBuffer; queryPool_:VkQueryPool; query_:T_uint32_t );
type PFN_vkCmdResetQueryPool                            = procedure( commandBuffer_:VkCommandBuffer; queryPool_:VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t );
type PFN_vkCmdWriteTimestamp                            = procedure( commandBuffer_:VkCommandBuffer; pipelineStage_:VkPipelineStageFlagBits; queryPool_:VkQueryPool; query_:T_uint32_t );
type PFN_vkCmdCopyQueryPoolResults                      = procedure( commandBuffer_:VkCommandBuffer; queryPool_:VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t; dstBuffer_:VkBuffer; dstOffset_:VkDeviceSize; stride_:VkDeviceSize; flags_:VkQueryResultFlags );
type PFN_vkCmdPushConstants                             = procedure( commandBuffer_:VkCommandBuffer; layout_:VkPipelineLayout; stageFlags_:VkShaderStageFlags; offset_:T_uint32_t; size_:T_uint32_t; const pValues_:P_void );
type PFN_vkCmdBeginRenderPass                           = procedure( commandBuffer_:VkCommandBuffer; const pRenderPassBegin_:P_VkRenderPassBeginInfo; contents_:VkSubpassContents );
type PFN_vkCmdNextSubpass                               = procedure( commandBuffer_:VkCommandBuffer; contents_:VkSubpassContents );
type PFN_vkCmdEndRenderPass                             = procedure( commandBuffer_:VkCommandBuffer );
type PFN_vkCmdExecuteCommands                           = procedure( commandBuffer_:VkCommandBuffer; commandBufferCount_:T_uint32_t; const pCommandBuffers_:P_VkCommandBuffer );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateInstance(
    pCreateInfo_ :P_VkInstanceCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pInstance_   :P_VkInstance ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyInstance(
    instance_   :VkInstance;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkEnumeratePhysicalDevices(
    instance_             :VkInstance;
    pPhysicalDeviceCount_ :P_uint32_t;
    pPhysicalDevices_     :P_VkPhysicalDevice ) :VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFeatures(
    physicalDevice_ :VkPhysicalDevice;
    pFeatures_      :P_VkPhysicalDeviceFeatures ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFormatProperties(
    physicalDevice_    :VkPhysicalDevice;
    format_            :VkFormat;
    pFormatProperties_ :P_VkFormatProperties ); stdcall; external DLLNAME;

function vkGetPhysicalDeviceImageFormatProperties(
    physicalDevice_:VkPhysicalDevice;
    format_                 :VkFormat;
    type_                   :VkImageType;
    tiling_                 :VkImageTiling;
    usage_                  :VkImageUsageFlags;
    flags_                  :VkImageCreateFlags;
    pImageFormatProperties_ :P_VkImageFormatProperties ) :VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceProperties(
    physicalDevice_ :VkPhysicalDevice;
    pProperties_    :P_VkPhysicalDeviceProperties ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceQueueFamilyProperties(
    physicalDevice_            :VkPhysicalDevice;
    pQueueFamilyPropertyCount_ :P_uint32_t;
    pQueueFamilyProperties_    :P_VkQueueFamilyProperties ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceMemoryProperties(
    physicalDevice_    :VkPhysicalDevice;
    pMemoryProperties_ :P_VkPhysicalDeviceMemoryProperties ); stdcall; external DLLNAME;

function vkGetInstanceProcAddr(
    instance_ :VkInstance;
    pName_    :P_char ) :PFN_vkVoidFunction; stdcall; external DLLNAME;

function vkGetDeviceProcAddr(
    device_ :VkDevice;
    pName_  :P_char ) :PFN_vkVoidFunction; stdcall; external DLLNAME;

function vkCreateDevice(
    physicalDevice_ :VkPhysicalDevice;
    pCreateInfo_    :P_VkDeviceCreateInfo;
    pAllocator_     :P_VkAllocationCallbacks;
    pDevice_        :P_VkDevice ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDevice(
    device_     :VkDevice;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkEnumerateInstanceExtensionProperties(
    pLayerName_     :P_char;
    pPropertyCount_ :P_uint32_t;
    pProperties_    :P_VkExtensionProperties ) :VkResult; stdcall; external DLLNAME;

function vkEnumerateDeviceExtensionProperties(
    physicalDevice_ :VkPhysicalDevice;
    pLayerName_     :P_char;
    pPropertyCount_ :P_uint32_t;
    pProperties_    :P_VkExtensionProperties ) :VkResult; stdcall; external DLLNAME;

function vkEnumerateInstanceLayerProperties(
    pPropertyCount_ :P_uint32_t;
    pProperties_    :P_VkLayerProperties ) :VkResult; stdcall; external DLLNAME;

function vkEnumerateDeviceLayerProperties(
    physicalDevice_ :VkPhysicalDevice;
    pPropertyCount_ :P_uint32_t;
    pProperties_    :P_VkLayerProperties ) :VkResult; stdcall; external DLLNAME;

procedure vkGetDeviceQueue(
    device_           :VkDevice;
    queueFamilyIndex_ :T_uint32_t;
    queueIndex_       :T_uint32_t;
    pQueue_           :P_VkQueue ); stdcall; external DLLNAME;

function vkQueueSubmit(
    queue_       :VkQueue;
    submitCount_ :T_uint32_t;
    pSubmits_    :P_VkSubmitInfo;
    fence_       :VkFence ) :VkResult; stdcall; external DLLNAME;

function vkQueueWaitIdle(
    queue_ :VkQueue ) :VkResult; stdcall; external DLLNAME;

function vkDeviceWaitIdle(
    device_ :VkDevice ) :VkResult; stdcall; external DLLNAME;

function vkAllocateMemory(
    device_        :VkDevice;
    pAllocateInfo_ :P_VkMemoryAllocateInfo;
    pAllocator_    :P_VkAllocationCallbacks;
    pMemory_       :P_VkDeviceMemory ) :VkResult; stdcall; external DLLNAME;

procedure vkFreeMemory(
    device_     :VkDevice;
    memory_     :VkDeviceMemory;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkMapMemory(
    device_ :VkDevice;
    memory_ :VkDeviceMemory;
    offset_ :VkDeviceSize;
    size_   :VkDeviceSize;
    flags_  :VkMemoryMapFlags;
    ppData_ :PP_void ) :VkResult; stdcall; external DLLNAME;

procedure vkUnmapMemory(
    device_ :VkDevice;
    memory_ :VkDeviceMemory ); stdcall; external DLLNAME;

function vkFlushMappedMemoryRanges(
    device_           :VkDevice;
    memoryRangeCount_ :T_uint32_t;
    pMemoryRanges_    :P_VkMappedMemoryRange ) :VkResult; stdcall; external DLLNAME;

function vkInvalidateMappedMemoryRanges(
    device_           :VkDevice;
    memoryRangeCount_ :T_uint32_t;
    pMemoryRanges_    :P_VkMappedMemoryRange ) :VkResult; stdcall; external DLLNAME;

procedure vkGetDeviceMemoryCommitment(
    device_                  :VkDevice;
    memory_                  :VkDeviceMemory;
    pCommittedMemoryInBytes_ :P_VkDeviceSize ); stdcall; external DLLNAME;

function vkBindBufferMemory(
    device_       :VkDevice;
    buffer_       :VkBuffer;
    memory_       :VkDeviceMemory;
    memoryOffset_ :VkDeviceSize ) :VkResult; stdcall; external DLLNAME;

function vkBindImageMemory(
    device_       :VkDevice;
    image_        :VkImage;
    memory_       :VkDeviceMemory;
    memoryOffset_ :VkDeviceSize ) :VkResult; stdcall; external DLLNAME;

procedure vkGetBufferMemoryRequirements(
    device_              :VkDevice;
    buffer_              :VkBuffer;
    pMemoryRequirements_ :P_VkMemoryRequirements ); stdcall; external DLLNAME;

procedure vkGetImageMemoryRequirements(
    device_              :VkDevice;
    image_               :VkImage;
    pMemoryRequirements_ :P_VkMemoryRequirements ); stdcall; external DLLNAME;

procedure vkGetImageSparseMemoryRequirements(
    device_                        :VkDevice;
    image_                         :VkImage;
    pSparseMemoryRequirementCount_ :P_uint32_t;
    pSparseMemoryRequirements_     :P_VkSparseImageMemoryRequirements ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceSparseImageFormatProperties(
    physicalDevice_ :VkPhysicalDevice;
    format_         :VkFormat;
    type_           :VkImageType;
    samples_        :VkSampleCountFlagBits;
    usage_          :VkImageUsageFlags;
    tiling_         :VkImageTiling;
    pPropertyCount_ :P_uint32_t;
    pProperties_    :P_VkSparseImageFormatProperties ); stdcall; external DLLNAME;

function vkQueueBindSparse(
    queue_         :VkQueue;
    bindInfoCount_ :T_uint32_t;
    pBindInfo_     :P_VkBindSparseInfo;
    fence_         :VkFence ) :VkResult; stdcall; external DLLNAME;

function vkCreateFence(
    device_      :VkDevice;
    pCreateInfo_ :P_VkFenceCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pFence_      :P_VkFence ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyFence(
    device_     :VkDevice;
    fence_      :VkFence;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkResetFences(
    device_     :VkDevice;
    fenceCount_ :T_uint32_t;
    pFences_    :P_VkFence ) :VkResult; stdcall; external DLLNAME;

function vkGetFenceStatus(
    device_ :VkDevice;
    fence_  :VkFence ) :VkResult; stdcall; external DLLNAME;

function vkWaitForFences(
    device_     :VkDevice;
    fenceCount_ :T_uint32_t;
    pFences_    :P_VkFence;
    waitAll_    :VkBool32;
    timeout_    :T_uint64_t ) :VkResult; stdcall; external DLLNAME;

function vkCreateSemaphore(
    device_      :VkDevice;
    pCreateInfo_ :P_VkSemaphoreCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pSemaphore_  :P_VkSemaphore ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroySemaphore(
    device_     :VkDevice;
    semaphore_  :VkSemaphore;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateEvent(
    device_      :VkDevice;
    pCreateInfo_ :P_VkEventCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pEvent_      :P_VkEvent ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyEvent(
    device_     :VkDevice;
    event_      :VkEvent;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetEventStatus(
    device_ :VkDevice;
    event_  :VkEvent ) :VkResult; stdcall; external DLLNAME;

function vkSetEvent(
    device_ :VkDevice;
    event_  :VkEvent ) :VkResult; stdcall; external DLLNAME;

function vkResetEvent(
    device_ :VkDevice;
    event_  :VkEvent ) :VkResult; stdcall; external DLLNAME;

function vkCreateQueryPool(
    device_      :VkDevice;
    pCreateInfo_ :P_VkQueryPoolCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pQueryPool_  :P_VkQueryPool ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyQueryPool(
    device_      :VkDevice;
    queryPool_   :VkQueryPool;
    pAllocator_  :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetQueryPoolResults(
    device_     :VkDevice;
    queryPool_  :VkQueryPool;
    firstQuery_ :T_uint32_t;
    queryCount_ :T_uint32_t;
    dataSize_   :T_size_t;
    pData_      :P_void;
    stride_     :VkDeviceSize;
    flags_      :VkQueryResultFlags ) :VkResult; stdcall; external DLLNAME;

function vkCreateBuffer(
    device_      :VkDevice;
    pCreateInfo_ :P_VkBufferCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pBuffer_     :P_VkBuffer ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyBuffer(
    device_     :VkDevice;
    buffer_     :VkBuffer;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateBufferView(
    device_      :VkDevice;
    pCreateInfo_ :P_VkBufferViewCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pView_       :P_VkBufferView ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyBufferView(
    device_     :VkDevice;
    bufferView_ :VkBufferView;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateImage(
    device_      :VkDevice;
    pCreateInfo_ :P_VkImageCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pImage_      :P_VkImage ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyImage(
    device_     :VkDevice;
    image_      :VkImage;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkGetImageSubresourceLayout(
    device_       :VkDevice;
    image_        :VkImage;
    pSubresource_ :P_VkImageSubresource;
    pLayout_      :P_VkSubresourceLayout ); stdcall; external DLLNAME;

function vkCreateImageView(
    device_      :VkDevice;
    pCreateInfo_ :P_VkImageViewCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pView_       :P_VkImageView ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyImageView(
    device_     :VkDevice;
    imageView_  :VkImageView;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateShaderModule(
    device_        :VkDevice;
    pCreateInfo_   :P_VkShaderModuleCreateInfo;
    pAllocator_    :P_VkAllocationCallbacks;
    pShaderModule_ :P_VkShaderModule ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyShaderModule(
    device_       :VkDevice;
    shaderModule_ :VkShaderModule;
    pAllocator_   :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreatePipelineCache(
    device_         :VkDevice;
    pCreateInfo_    :P_VkPipelineCacheCreateInfo;
    pAllocator_     :P_VkAllocationCallbacks;
    pPipelineCache_ :P_VkPipelineCache ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyPipelineCache(
    device_        :VkDevice;
    pipelineCache_ :VkPipelineCache;
    pAllocator_    :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetPipelineCacheData(
    device_        :VkDevice;
    pipelineCache_ :VkPipelineCache;
    pDataSize_     :P_size_t;
    pData_         :P_void ) :VkResult; stdcall; external DLLNAME;

function vkMergePipelineCaches(
    device_        :VkDevice;
    dstCache_      :VkPipelineCache;
    srcCacheCount_ :T_uint32_t;
    pSrcCaches_    :P_VkPipelineCache ) :VkResult; stdcall; external DLLNAME;

function vkCreateGraphicsPipelines(
    device_          :VkDevice;
    pipelineCache_   :VkPipelineCache;
    createInfoCount_ :T_uint32_t;
    pCreateInfos_    :P_VkGraphicsPipelineCreateInfo;
    pAllocator_      :P_VkAllocationCallbacks;
    pPipelines_      :P_VkPipeline ) :VkResult; stdcall; external DLLNAME;

function vkCreateComputePipelines(
    device_          :VkDevice;
    pipelineCache_   :VkPipelineCache;
    createInfoCount_ :T_uint32_t;
    pCreateInfos_    :P_VkComputePipelineCreateInfo;
    pAllocator_      :P_VkAllocationCallbacks;
    pPipelines_      :P_VkPipeline ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyPipeline(
    device_     :VkDevice;
    pipeline_   :VkPipeline;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreatePipelineLayout(
    device_          :VkDevice;
    pCreateInfo_     :P_VkPipelineLayoutCreateInfo;
    pAllocator_      :P_VkAllocationCallbacks;
    pPipelineLayout_ :P_VkPipelineLayout ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyPipelineLayout(
    device_         :VkDevice;
    pipelineLayout_ :VkPipelineLayout;
    pAllocator_     :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateSampler(
    device_      :VkDevice;
    pCreateInfo_ :P_VkSamplerCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pSampler_    :P_VkSampler ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroySampler(
    device_     :VkDevice;
    sampler_    :VkSampler;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateDescriptorSetLayout(
    device_      :VkDevice;
    pCreateInfo_ :P_VkDescriptorSetLayoutCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pSetLayout_  :P_VkDescriptorSetLayout ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDescriptorSetLayout(
    device_              :VkDevice;
    descriptorSetLayout_ :VkDescriptorSetLayout;
    pAllocator_          :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateDescriptorPool(
    device_          :VkDevice;
    pCreateInfo_     :P_VkDescriptorPoolCreateInfo;
    pAllocator_      :P_VkAllocationCallbacks;
    pDescriptorPool_ :P_VkDescriptorPool ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDescriptorPool(
    device_         :VkDevice;
    descriptorPool_ :VkDescriptorPool;
    pAllocator_     :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkResetDescriptorPool(
    device_         :VkDevice;
    descriptorPool_ :VkDescriptorPool;
    flags_          :VkDescriptorPoolResetFlags ) :VkResult; stdcall; external DLLNAME;

function vkAllocateDescriptorSets(
    device_          :VkDevice;
    pAllocateInfo_   :P_VkDescriptorSetAllocateInfo;
    pDescriptorSets_ :P_VkDescriptorSet ) :VkResult; stdcall; external DLLNAME;

function vkFreeDescriptorSets(
    device_             :VkDevice;
    descriptorPool_     :VkDescriptorPool;
    descriptorSetCount_ :T_uint32_t;
    pDescriptorSets_    :P_VkDescriptorSet ) :VkResult; stdcall; external DLLNAME;

procedure vkUpdateDescriptorSets(
    device_               :VkDevice;
    descriptorWriteCount_ :T_uint32_t;
    pDescriptorWrites_    :P_VkWriteDescriptorSet;
    descriptorCopyCount_  :T_uint32_t;
    pDescriptorCopies_    :P_VkCopyDescriptorSet ); stdcall; external DLLNAME;

function vkCreateFramebuffer(
    device_       :VkDevice;
    pCreateInfo_  :P_VkFramebufferCreateInfo;
    pAllocator_   :P_VkAllocationCallbacks;
    pFramebuffer_ :P_VkFramebuffer ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyFramebuffer(
    device_      :VkDevice;
    framebuffer_ :VkFramebuffer;
    pAllocator_  :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateRenderPass(
    device_      :VkDevice;
    pCreateInfo_ :P_VkRenderPassCreateInfo;
    pAllocator_  :P_VkAllocationCallbacks;
    pRenderPass_ :P_VkRenderPass ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyRenderPass(
    device_     :VkDevice;
    renderPass_ :VkRenderPass;
    pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkGetRenderAreaGranularity(
    device_       :VkDevice;
    renderPass_   :VkRenderPass;
    pGranularity_ :P_VkExtent2D ); stdcall; external DLLNAME;

function vkCreateCommandPool(
    device_       :VkDevice;
    pCreateInfo_  :P_VkCommandPoolCreateInfo;
    pAllocator_   :P_VkAllocationCallbacks;
    pCommandPool_ :P_VkCommandPool ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyCommandPool(
    device_      :VkDevice;
    commandPool_ :VkCommandPool;
    pAllocator_  :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkResetCommandPool(
    device_      :VkDevice;
    commandPool_ :VkCommandPool;
    flags_       :VkCommandPoolResetFlags ) :VkResult; stdcall; external DLLNAME;

function vkAllocateCommandBuffers(
    device_          :VkDevice;
    pAllocateInfo_   :P_VkCommandBufferAllocateInfo;
    pCommandBuffers_ :P_VkCommandBuffer ) :VkResult; stdcall; external DLLNAME;

procedure vkFreeCommandBuffers(
    device_             :VkDevice;
    commandPool_        :VkCommandPool;
    commandBufferCount_ :T_uint32_t;
    pCommandBuffers_    :P_VkCommandBuffer ); stdcall; external DLLNAME;

function vkBeginCommandBuffer(
    commandBuffer_ :VkCommandBuffer;
    pBeginInfo_    :P_VkCommandBufferBeginInfo ) :VkResult; stdcall; external DLLNAME;

function vkEndCommandBuffer(
    commandBuffer_ :VkCommandBuffer ) :VkResult; stdcall; external DLLNAME;

function vkResetCommandBuffer(
    commandBuffer_ :VkCommandBuffer;
    flags_         :VkCommandBufferResetFlags ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdBindPipeline(
    commandBuffer_     :VkCommandBuffer;
    pipelineBindPoint_ :VkPipelineBindPoint;
    pipeline_          :VkPipeline ); stdcall; external DLLNAME;

procedure vkCmdSetViewport(
    commandBuffer_ :VkCommandBuffer;
    firstViewport_ :T_uint32_t;
    viewportCount_ :T_uint32_t;
    pViewports_    :P_VkViewport ); stdcall; external DLLNAME;

procedure vkCmdSetScissor(
    commandBuffer_ :VkCommandBuffer;
    firstScissor_  :T_uint32_t;
    scissorCount_  :T_uint32_t;
    pScissors_     :P_VkRect2D ); stdcall; external DLLNAME;

procedure vkCmdSetLineWidth(
    commandBuffer_ :VkCommandBuffer;
    lineWidth_     :T_float ); stdcall; external DLLNAME;

procedure vkCmdSetDepthBias(
    commandBuffer_           :VkCommandBuffer;
    depthBiasConstantFactor_ :T_float;
    depthBiasClamp_          :T_float;
    depthBiasSlopeFactor_    :T_float ); stdcall; external DLLNAME;

procedure vkCmdSetBlendConstants(
    commandBuffer_        :VkCommandBuffer;
    const blendConstants_ :T_blendConstants ); stdcall; external DLLNAME;

procedure vkCmdSetDepthBounds(
    commandBuffer_  :VkCommandBuffer;
    minDepthBounds_ :T_float;
    maxDepthBounds_ :T_float ); stdcall; external DLLNAME;

procedure vkCmdSetStencilCompareMask(
    commandBuffer_ :VkCommandBuffer;
    faceMask_      :VkStencilFaceFlags;
    compareMask_   :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdSetStencilWriteMask(
    commandBuffer_ :VkCommandBuffer;
    faceMask_      :VkStencilFaceFlags;
    writeMask_     :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdSetStencilReference(
    commandBuffer_ :VkCommandBuffer;
    faceMask_      :VkStencilFaceFlags;
    reference_     :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdBindDescriptorSets(
    commandBuffer_      :VkCommandBuffer;
    pipelineBindPoint_  :VkPipelineBindPoint;
    layout_             :VkPipelineLayout;
    firstSet_           :T_uint32_t;
    descriptorSetCount_ :T_uint32_t;
    pDescriptorSets_    :P_VkDescriptorSet;
    dynamicOffsetCount_ :T_uint32_t;
    pDynamicOffsets_    :P_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdBindIndexBuffer(
    commandBuffer_ :VkCommandBuffer;
    buffer_        :VkBuffer;
    offset_        :VkDeviceSize;
    indexType_     :VkIndexType ); stdcall; external DLLNAME;

procedure vkCmdBindVertexBuffers(
    commandBuffer_ :VkCommandBuffer;
    firstBinding_  :T_uint32_t;
    bindingCount_  :T_uint32_t;
    pBuffers_      :P_VkBuffer;
    pOffsets_      :P_VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdDraw(
    commandBuffer_ :VkCommandBuffer;
    vertexCount_   :T_uint32_t;
    instanceCount_ :T_uint32_t;
    firstVertex_   :T_uint32_t;
    firstInstance_ :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexed(
    commandBuffer_ :VkCommandBuffer;
    indexCount_    :T_uint32_t;
    instanceCount_ :T_uint32_t;
    firstIndex_    :T_uint32_t;
    vertexOffset_  :T_int32_t;
    firstInstance_ :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndirect(
    commandBuffer_ :VkCommandBuffer;
    buffer_        :VkBuffer;
    offset_        :VkDeviceSize;
    drawCount_     :T_uint32_t;
    stride_        :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexedIndirect(
    commandBuffer_ :VkCommandBuffer;
    buffer_        :VkBuffer;
    offset_        :VkDeviceSize;
    drawCount_     :T_uint32_t;
    stride_        :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDispatch(
    commandBuffer_ :VkCommandBuffer;
    groupCountX_   :T_uint32_t;
    groupCountY_   :T_uint32_t;
    groupCountZ_   :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDispatchIndirect(
    commandBuffer_ :VkCommandBuffer;
    buffer_        :VkBuffer;
    offset_        :VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdCopyBuffer(
    commandBuffer_ :VkCommandBuffer;
    srcBuffer_     :VkBuffer;
    dstBuffer_     :VkBuffer;
    regionCount_   :T_uint32_t;
    pRegions_      :P_VkBufferCopy ); stdcall; external DLLNAME;

procedure vkCmdCopyImage(
    commandBuffer_  :VkCommandBuffer;
    srcImage_       :VkImage;
    srcImageLayout_ :VkImageLayout;
    dstImage_       :VkImage;
    dstImageLayout_ :VkImageLayout;
    regionCount_    :T_uint32_t;
    pRegions_       :P_VkImageCopy ); stdcall; external DLLNAME;

procedure vkCmdBlitImage(
    commandBuffer_  :VkCommandBuffer;
    srcImage_       :VkImage;
    srcImageLayout_ :VkImageLayout;
    dstImage_       :VkImage;
    dstImageLayout_ :VkImageLayout;
    regionCount_    :T_uint32_t;
    pRegions_       :P_VkImageBlit;
    filter_         :VkFilter ); stdcall; external DLLNAME;

procedure vkCmdCopyBufferToImage(
    commandBuffer_  :VkCommandBuffer;
    srcBuffer_      :VkBuffer;
    dstImage_       :VkImage;
    dstImageLayout_ :VkImageLayout;
    regionCount_    :T_uint32_t;
    pRegions_       :P_VkBufferImageCopy ); stdcall; external DLLNAME;

procedure vkCmdCopyImageToBuffer(
    commandBuffer_  :VkCommandBuffer;
    srcImage_       :VkImage;
    srcImageLayout_ :VkImageLayout;
    dstBuffer_      :VkBuffer;
    regionCount_    :T_uint32_t;
    pRegions_       :P_VkBufferImageCopy ); stdcall; external DLLNAME;

procedure vkCmdUpdateBuffer(
    commandBuffer_ :VkCommandBuffer;
    dstBuffer_     :VkBuffer;
    dstOffset_     :VkDeviceSize;
    dataSize_      :VkDeviceSize;
    pData_         :P_void ); stdcall; external DLLNAME;

procedure vkCmdFillBuffer(
    commandBuffer_ :VkCommandBuffer;
    dstBuffer_     :VkBuffer;
    dstOffset_     :VkDeviceSize;
    size_          :VkDeviceSize;
    data_          :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdClearColorImage(
    commandBuffer_ :VkCommandBuffer;
    image_         :VkImage;
    imageLayout_   :VkImageLayout;
    pColor_        :P_VkClearColorValue;
    rangeCount_    :T_uint32_t;
    pRanges_       :P_VkImageSubresourceRange ); stdcall; external DLLNAME;

procedure vkCmdClearDepthStencilImage(
    commandBuffer_ :VkCommandBuffer;
    image_         :VkImage;
    imageLayout_   :VkImageLayout;
    pDepthStencil_ :P_VkClearDepthStencilValue;
    rangeCount_    :T_uint32_t;
    pRanges_       :P_VkImageSubresourceRange ); stdcall; external DLLNAME;

procedure vkCmdClearAttachments(
    commandBuffer_   :VkCommandBuffer;
    attachmentCount_ :T_uint32_t;
    pAttachments_    :P_VkClearAttachment;
    rectCount_       :T_uint32_t;
    pRects_          :P_VkClearRect ); stdcall; external DLLNAME;

procedure vkCmdResolveImage(
    commandBuffer_  :VkCommandBuffer;
    srcImage_       :VkImage;
    srcImageLayout_ :VkImageLayout;
    dstImage_       :VkImage;
    dstImageLayout_ :VkImageLayout;
    regionCount_    :T_uint32_t;
    pRegions_       :P_VkImageResolve ); stdcall; external DLLNAME;

procedure vkCmdSetEvent(
    commandBuffer_ :VkCommandBuffer;
    event_         :VkEvent;
    stageMask_     :VkPipelineStageFlags ); stdcall; external DLLNAME;

procedure vkCmdResetEvent(
    commandBuffer_ :VkCommandBuffer;
    event_         :VkEvent;
    stageMask_     :VkPipelineStageFlags ); stdcall; external DLLNAME;

procedure vkCmdWaitEvents(
    commandBuffer_            :VkCommandBuffer;
    eventCount_               :T_uint32_t;
    pEvents_                  :P_VkEvent;
    srcStageMask_             :VkPipelineStageFlags;
    dstStageMask_             :VkPipelineStageFlags;
    memoryBarrierCount_       :T_uint32_t;
    pMemoryBarriers_          :P_VkMemoryBarrier;
    bufferMemoryBarrierCount_ :T_uint32_t;
    pBufferMemoryBarriers_    :P_VkBufferMemoryBarrier;
    imageMemoryBarrierCount_  :T_uint32_t;
    pImageMemoryBarriers_     :P_VkImageMemoryBarrier ); stdcall; external DLLNAME;

procedure vkCmdPipelineBarrier(
    commandBuffer_            :VkCommandBuffer;
    srcStageMask_             :VkPipelineStageFlags;
    dstStageMask_             :VkPipelineStageFlags;
    dependencyFlags_          :VkDependencyFlags;
    memoryBarrierCount_       :T_uint32_t;
    pMemoryBarriers_          :P_VkMemoryBarrier;
    bufferMemoryBarrierCount_ :T_uint32_t;
    pBufferMemoryBarriers_    :P_VkBufferMemoryBarrier;
    imageMemoryBarrierCount_  :T_uint32_t;
    pImageMemoryBarriers_     :P_VkImageMemoryBarrier ); stdcall; external DLLNAME;

procedure vkCmdBeginQuery(
    commandBuffer_ :VkCommandBuffer;
    queryPool_     :VkQueryPool;
    query_         :T_uint32_t;
    flags_         :VkQueryControlFlags ); stdcall; external DLLNAME;

procedure vkCmdEndQuery(
    commandBuffer_ :VkCommandBuffer;
    queryPool_     :VkQueryPool;
    query_         :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdResetQueryPool(
    commandBuffer_ :VkCommandBuffer;
    queryPool_     :VkQueryPool;
    firstQuery_    :T_uint32_t;
    queryCount_    :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdWriteTimestamp(
    commandBuffer_ :VkCommandBuffer;
    pipelineStage_ :VkPipelineStageFlagBits;
    queryPool_     :VkQueryPool;
    query_         :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdCopyQueryPoolResults(
    commandBuffer_ :VkCommandBuffer;
    queryPool_     :VkQueryPool;
    firstQuery_    :T_uint32_t;
    queryCount_    :T_uint32_t;
    dstBuffer_     :VkBuffer;
    dstOffset_     :VkDeviceSize;
    stride_        :VkDeviceSize;
    flags_         :VkQueryResultFlags ); stdcall; external DLLNAME;

procedure vkCmdPushConstants(
    commandBuffer_ :VkCommandBuffer;
    layout_        :VkPipelineLayout;
    stageFlags_    :VkShaderStageFlags;
    offset_        :T_uint32_t;
    size_          :T_uint32_t;
    pValues_       :P_void ); stdcall; external DLLNAME;

procedure vkCmdBeginRenderPass(
    commandBuffer_    :VkCommandBuffer;
    pRenderPassBegin_ :P_VkRenderPassBeginInfo;
    contents_         :VkSubpassContents ); stdcall; external DLLNAME;

procedure vkCmdNextSubpass(
    commandBuffer_ :VkCommandBuffer;
    contents_      :VkSubpassContents ); stdcall; external DLLNAME;

procedure vkCmdEndRenderPass(
    commandBuffer_ :VkCommandBuffer ); stdcall; external DLLNAME;

procedure vkCmdExecuteCommands(
    commandBuffer_      :VkCommandBuffer;
    commandBufferCount_ :T_uint32_t;
    pCommandBuffers_    :P_VkCommandBuffer ); stdcall; external DLLNAME;
{$ENDIF}


const VK_VERSION_1_1     = 1;
// Vulkan 1.1 version number
const VK_API_VERSION_1_1 = {VK_MAKE_VERSION( 1, 1, 0 )} ( 1 shl 22 ) or ( 1 shl 12 ) or 0; // Patch version should always be set to 0

type P_VkSamplerYcbcrConversion   = ^VkSamplerYcbcrConversion;
     VkSamplerYcbcrConversion   = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
type P_VkDescriptorUpdateTemplate = ^VkDescriptorUpdateTemplate;
     VkDescriptorUpdateTemplate = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_MAX_DEVICE_GROUP_SIZE = 32;
const VK_LUID_SIZE             = 8;
const VK_QUEUE_FAMILY_EXTERNAL = UInt32( $FFFFFFFF )-1; {(~0U-1)}

type P_VkPointClippingBehavior = ^VkPointClippingBehavior;
     VkPointClippingBehavior = (
       VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES           = 0,
       VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY     = 1,
       VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR       = VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES,
       VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR = VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY,
       VK_POINT_CLIPPING_BEHAVIOR_MAX_ENUM                  = $7FFFFFFF
     );

type P_VkTessellationDomainOrigin = ^VkTessellationDomainOrigin;
     VkTessellationDomainOrigin = (
       VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT     = 0,
       VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT     = 1,
       VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR = VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT,
       VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR = VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT,
       VK_TESSELLATION_DOMAIN_ORIGIN_MAX_ENUM       = $7FFFFFFF
     );

type P_VkSamplerYcbcrModelConversion = ^VkSamplerYcbcrModelConversion;
     VkSamplerYcbcrModelConversion = (
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY       = 0,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY     = 1,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709          = 2,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601          = 3,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020         = 4,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR   = VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR      = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR      = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR     = VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020,
       VK_SAMPLER_YCBCR_MODEL_CONVERSION_MAX_ENUM           = $7FFFFFFF
     );

type P_VkSamplerYcbcrRange = ^VkSamplerYcbcrRange;
     VkSamplerYcbcrRange = (
       VK_SAMPLER_YCBCR_RANGE_ITU_FULL       = 0,
       VK_SAMPLER_YCBCR_RANGE_ITU_NARROW     = 1,
       VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR   = VK_SAMPLER_YCBCR_RANGE_ITU_FULL,
       VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR = VK_SAMPLER_YCBCR_RANGE_ITU_NARROW,
       VK_SAMPLER_YCBCR_RANGE_MAX_ENUM       = $7FFFFFFF
     );

type P_VkChromaLocation = ^VkChromaLocation;
     VkChromaLocation = (
       VK_CHROMA_LOCATION_COSITED_EVEN     = 0,
       VK_CHROMA_LOCATION_MIDPOINT         = 1,
       VK_CHROMA_LOCATION_COSITED_EVEN_KHR = VK_CHROMA_LOCATION_COSITED_EVEN,
       VK_CHROMA_LOCATION_MIDPOINT_KHR     = VK_CHROMA_LOCATION_MIDPOINT,
       VK_CHROMA_LOCATION_MAX_ENUM         = $7FFFFFFF
     );

type P_VkDescriptorUpdateTemplateType = ^VkDescriptorUpdateTemplateType;
     VkDescriptorUpdateTemplateType = (
       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET       = 0,
       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = 1,
       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR   = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET,
       VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_MAX_ENUM             = $7FFFFFFF
     );

type P_VkSubgroupFeatureFlagBits = ^VkSubgroupFeatureFlagBits;
     VkSubgroupFeatureFlagBits = (
       VK_SUBGROUP_FEATURE_BASIC_BIT            = $00000001,
       VK_SUBGROUP_FEATURE_VOTE_BIT             = $00000002,
       VK_SUBGROUP_FEATURE_ARITHMETIC_BIT       = $00000004,
       VK_SUBGROUP_FEATURE_BALLOT_BIT           = $00000008,
       VK_SUBGROUP_FEATURE_SHUFFLE_BIT          = $00000010,
       VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = $00000020,
       VK_SUBGROUP_FEATURE_CLUSTERED_BIT        = $00000040,
       VK_SUBGROUP_FEATURE_QUAD_BIT             = $00000080,
       VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV   = $00000100,
       VK_SUBGROUP_FEATURE_FLAG_BITS_MAX_ENUM   = $7FFFFFFF
     );
type P_VkSubgroupFeatureFlags = ^VkSubgroupFeatureFlags;
     VkSubgroupFeatureFlags = VkFlags;

type P_VkPeerMemoryFeatureFlagBits = ^VkPeerMemoryFeatureFlagBits;
     VkPeerMemoryFeatureFlagBits = (
       VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT        = $00000001,
       VK_PEER_MEMORY_FEATURE_COPY_DST_BIT        = $00000002,
       VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT     = $00000004,
       VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT     = $00000008,
       VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR    = VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT,
       VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR    = VK_PEER_MEMORY_FEATURE_COPY_DST_BIT,
       VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR = VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT,
       VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR = VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT,
       VK_PEER_MEMORY_FEATURE_FLAG_BITS_MAX_ENUM  = $7FFFFFFF
     );
type P_VkPeerMemoryFeatureFlags = ^VkPeerMemoryFeatureFlags;
     VkPeerMemoryFeatureFlags = VkFlags;

type P_VkMemoryAllocateFlagBits = ^VkMemoryAllocateFlagBits;
     VkMemoryAllocateFlagBits = (
       VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT                       = $00000001,
       VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT                    = $00000002,
       VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT     = $00000004,
       VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR                   = VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT,
       VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR                = VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT,
       VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR = VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
       VK_MEMORY_ALLOCATE_FLAG_BITS_MAX_ENUM                    = $7FFFFFFF
     );
type P_VkMemoryAllocateFlags                 = ^VkMemoryAllocateFlags;
     VkMemoryAllocateFlags                 = VkFlags;
type P_VkCommandPoolTrimFlags                = ^VkCommandPoolTrimFlags;
     VkCommandPoolTrimFlags                = VkFlags;
type P_VkDescriptorUpdateTemplateCreateFlags = ^VkDescriptorUpdateTemplateCreateFlags;
     VkDescriptorUpdateTemplateCreateFlags = VkFlags;

type P_VkExternalMemoryHandleTypeFlagBits = ^VkExternalMemoryHandleTypeFlagBits;
     VkExternalMemoryHandleTypeFlagBits = (
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT                       = $00000001,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT                    = $00000002,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT                = $00000004,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT                   = $00000008,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT               = $00000010,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT                      = $00000020,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT                  = $00000040,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT                     = $00000200,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID = $00000400,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT             = $00000080,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT  = $00000100,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR                   = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR                = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR            = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR               = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR           = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR                  = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR              = VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_FLAG_BITS_MAX_ENUM                  = $7FFFFFFF
     );
type P_VkExternalMemoryHandleTypeFlags = ^VkExternalMemoryHandleTypeFlags;
     VkExternalMemoryHandleTypeFlags = VkFlags;

type P_VkExternalMemoryFeatureFlagBits = ^VkExternalMemoryFeatureFlagBits;
     VkExternalMemoryFeatureFlagBits = (
       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT     = $00000001,
       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT         = $00000002,
       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT         = $00000004,
       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR = VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT,
       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR     = VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT,
       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR     = VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT,
       VK_EXTERNAL_MEMORY_FEATURE_FLAG_BITS_MAX_ENUM     = $7FFFFFFF
     );
type P_VkExternalMemoryFeatureFlags = ^VkExternalMemoryFeatureFlags;
     VkExternalMemoryFeatureFlags = VkFlags;

type P_VkExternalFenceHandleTypeFlagBits = ^VkExternalFenceHandleTypeFlagBits;
     VkExternalFenceHandleTypeFlagBits = (
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT            = $00000001,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT         = $00000002,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT     = $00000004,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT              = $00000008,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR        = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR     = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR          = VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT,
       VK_EXTERNAL_FENCE_HANDLE_TYPE_FLAG_BITS_MAX_ENUM       = $7FFFFFFF
     );
type P_VkExternalFenceHandleTypeFlags = ^VkExternalFenceHandleTypeFlags;
     VkExternalFenceHandleTypeFlags = VkFlags;

type P_VkExternalFenceFeatureFlagBits = ^VkExternalFenceFeatureFlagBits;
     VkExternalFenceFeatureFlagBits = (
       VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT     = $00000001,
       VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT     = $00000002,
       VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT,
       VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT,
       VK_EXTERNAL_FENCE_FEATURE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkExternalFenceFeatureFlags = ^VkExternalFenceFeatureFlags;
     VkExternalFenceFeatureFlags = VkFlags;

type P_VkFenceImportFlagBits = ^VkFenceImportFlagBits;
     VkFenceImportFlagBits = (
       VK_FENCE_IMPORT_TEMPORARY_BIT      = $00000001,
       VK_FENCE_IMPORT_TEMPORARY_BIT_KHR  = VK_FENCE_IMPORT_TEMPORARY_BIT,
       VK_FENCE_IMPORT_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkFenceImportFlags = ^VkFenceImportFlags;
     VkFenceImportFlags = VkFlags;

type P_VkSemaphoreImportFlagBits = ^VkSemaphoreImportFlagBits;
     VkSemaphoreImportFlagBits = (
       VK_SEMAPHORE_IMPORT_TEMPORARY_BIT      = $00000001,
       VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR  = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT,
       VK_SEMAPHORE_IMPORT_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkSemaphoreImportFlags = ^VkSemaphoreImportFlags;
     VkSemaphoreImportFlags = VkFlags;

type P_VkExternalSemaphoreHandleTypeFlagBits = ^VkExternalSemaphoreHandleTypeFlagBits;
     VkExternalSemaphoreHandleTypeFlagBits = (
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT            = $00000001,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT         = $00000002,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT     = $00000004,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT          = $00000008,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT              = $00000010,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D11_FENCE_BIT          = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR        = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR     = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR      = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR          = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT,
       VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_FLAG_BITS_MAX_ENUM       = $7FFFFFFF
     );
type P_VkExternalSemaphoreHandleTypeFlags = ^VkExternalSemaphoreHandleTypeFlags;
     VkExternalSemaphoreHandleTypeFlags = VkFlags;

type P_VkExternalSemaphoreFeatureFlagBits = ^VkExternalSemaphoreFeatureFlagBits;
     VkExternalSemaphoreFeatureFlagBits = (
       VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT     = $00000001,
       VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT     = $00000002,
       VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT,
       VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT,
       VK_EXTERNAL_SEMAPHORE_FEATURE_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkExternalSemaphoreFeatureFlags = ^VkExternalSemaphoreFeatureFlags;
     VkExternalSemaphoreFeatureFlags = VkFlags;
type P_VkPhysicalDeviceSubgroupProperties = ^VkPhysicalDeviceSubgroupProperties;
     VkPhysicalDeviceSubgroupProperties = record
       sType                     :VkStructureType;
       pNext                     :P_void;
       subgroupSize              :T_uint32_t;
       supportedStages           :VkShaderStageFlags;
       supportedOperations       :VkSubgroupFeatureFlags;
       quadOperationsInAllStages :VkBool32;
     end;

type P_VkBindBufferMemoryInfo = ^VkBindBufferMemoryInfo;
     VkBindBufferMemoryInfo = record
       sType        :VkStructureType;
       pNext        :P_void;
       buffer       :VkBuffer;
       memory       :VkDeviceMemory;
       memoryOffset :VkDeviceSize;
     end;

type P_VkBindImageMemoryInfo = ^VkBindImageMemoryInfo;
     VkBindImageMemoryInfo = record
       sType        :VkStructureType;
       pNext        :P_void;
       image        :VkImage;
       memory       :VkDeviceMemory;
       memoryOffset :VkDeviceSize;
     end;

type P_VkPhysicalDevice16BitStorageFeatures = ^VkPhysicalDevice16BitStorageFeatures;
     VkPhysicalDevice16BitStorageFeatures = record
       sType                              :VkStructureType;
       pNext                              :P_void;
       storageBuffer16BitAccess           :VkBool32;
       uniformAndStorageBuffer16BitAccess :VkBool32;
       storagePushConstant16              :VkBool32;
       storageInputOutput16               :VkBool32;
     end;

type P_VkMemoryDedicatedRequirements = ^VkMemoryDedicatedRequirements;
     VkMemoryDedicatedRequirements = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       prefersDedicatedAllocation  :VkBool32;
       requiresDedicatedAllocation :VkBool32;
     end;

type P_VkMemoryDedicatedAllocateInfo = ^VkMemoryDedicatedAllocateInfo;
     VkMemoryDedicatedAllocateInfo = record
       sType  :VkStructureType;
       pNext  :P_void;
       image  :VkImage;
       buffer :VkBuffer;
     end;

type P_VkMemoryAllocateFlagsInfo = ^VkMemoryAllocateFlagsInfo;
     VkMemoryAllocateFlagsInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       flags      :VkMemoryAllocateFlags;
       deviceMask :T_uint32_t;
     end;

type P_VkDeviceGroupRenderPassBeginInfo = ^VkDeviceGroupRenderPassBeginInfo;
     VkDeviceGroupRenderPassBeginInfo = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       deviceMask            :T_uint32_t;
       deviceRenderAreaCount :T_uint32_t;
       pDeviceRenderAreas    :P_VkRect2D;
     end;

type P_VkDeviceGroupCommandBufferBeginInfo = ^VkDeviceGroupCommandBufferBeginInfo;
     VkDeviceGroupCommandBufferBeginInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       deviceMask :T_uint32_t;
     end;

type P_VkDeviceGroupSubmitInfo = ^VkDeviceGroupSubmitInfo;
     VkDeviceGroupSubmitInfo = record
       sType                         :VkStructureType;
       pNext                         :P_void;
       waitSemaphoreCount            :T_uint32_t;
       pWaitSemaphoreDeviceIndices   :P_uint32_t;
       commandBufferCount            :T_uint32_t;
       pCommandBufferDeviceMasks     :P_uint32_t;
       signalSemaphoreCount          :T_uint32_t;
       pSignalSemaphoreDeviceIndices :P_uint32_t;
     end;

type P_VkDeviceGroupBindSparseInfo = ^VkDeviceGroupBindSparseInfo;
     VkDeviceGroupBindSparseInfo = record
       sType               :VkStructureType;
       pNext               :P_void;
       resourceDeviceIndex :T_uint32_t;
       memoryDeviceIndex   :T_uint32_t;
     end;

type P_VkBindBufferMemoryDeviceGroupInfo = ^VkBindBufferMemoryDeviceGroupInfo;
     VkBindBufferMemoryDeviceGroupInfo = record
       sType            :VkStructureType;
       pNext            :P_void;
       deviceIndexCount :T_uint32_t;
       pDeviceIndices   :P_uint32_t;
     end;

type P_VkBindImageMemoryDeviceGroupInfo = ^VkBindImageMemoryDeviceGroupInfo;
     VkBindImageMemoryDeviceGroupInfo = record
       sType                        :VkStructureType;
       pNext                        :P_void;
       deviceIndexCount             :T_uint32_t;
       pDeviceIndices               :P_uint32_t;
       splitInstanceBindRegionCount :T_uint32_t;
       pSplitInstanceBindRegions    :P_VkRect2D;
     end;

type P_VkPhysicalDeviceGroupProperties = ^VkPhysicalDeviceGroupProperties;
     VkPhysicalDeviceGroupProperties = record
       sType               :VkStructureType;
       pNext               :P_void;
       physicalDeviceCount :T_uint32_t;
       physicalDevices     :array [ 0..VK_MAX_DEVICE_GROUP_SIZE-1 ] of VkPhysicalDevice;
       subsetAllocation    :VkBool32;
     end;

type P_VkDeviceGroupDeviceCreateInfo = ^VkDeviceGroupDeviceCreateInfo;
     VkDeviceGroupDeviceCreateInfo = record
       sType               :VkStructureType;
       pNext               :P_void;
       physicalDeviceCount :T_uint32_t;
       pPhysicalDevices    :P_VkPhysicalDevice;
     end;

type P_VkBufferMemoryRequirementsInfo2 = ^VkBufferMemoryRequirementsInfo2;
     VkBufferMemoryRequirementsInfo2 = record
       sType  :VkStructureType;
       pNext  :P_void;
       buffer :VkBuffer;
     end;

type P_VkImageMemoryRequirementsInfo2 = ^VkImageMemoryRequirementsInfo2;
     VkImageMemoryRequirementsInfo2 = record
       sType :VkStructureType;
       pNext :P_void;
       image :VkImage;
     end;

type P_VkImageSparseMemoryRequirementsInfo2 = ^VkImageSparseMemoryRequirementsInfo2;
     VkImageSparseMemoryRequirementsInfo2 = record
       sType :VkStructureType;
       pNext :P_void;
       image :VkImage;
     end;

type P_VkMemoryRequirements2 = ^VkMemoryRequirements2;
     VkMemoryRequirements2 = record
       sType              :VkStructureType;
       pNext              :P_void;
       memoryRequirements :VkMemoryRequirements;
     end;

type P_VkSparseImageMemoryRequirements2 = ^VkSparseImageMemoryRequirements2;
     VkSparseImageMemoryRequirements2 = record
       sType              :VkStructureType;
       pNext              :P_void;
       memoryRequirements :VkSparseImageMemoryRequirements;
     end;

type P_VkPhysicalDeviceFeatures2 = ^VkPhysicalDeviceFeatures2;
     VkPhysicalDeviceFeatures2 = record
       sType    :VkStructureType;
       pNext    :P_void;
       features :VkPhysicalDeviceFeatures;
     end;

type P_VkPhysicalDeviceProperties2 = ^VkPhysicalDeviceProperties2;
     VkPhysicalDeviceProperties2 = record
       sType      :VkStructureType;
       pNext      :P_void;
       properties :VkPhysicalDeviceProperties;
     end;

type P_VkFormatProperties2 = ^VkFormatProperties2;
     VkFormatProperties2 = record
       sType            :VkStructureType;
       pNext            :P_void;
       formatProperties :VkFormatProperties;
     end;

type P_VkImageFormatProperties2 = ^VkImageFormatProperties2;
     VkImageFormatProperties2 = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       imageFormatProperties :VkImageFormatProperties;
     end;

type P_VkPhysicalDeviceImageFormatInfo2 = ^VkPhysicalDeviceImageFormatInfo2;
     VkPhysicalDeviceImageFormatInfo2 = record
       sType  :VkStructureType;
       pNext  :P_void;
       format :VkFormat;
       type_  :VkImageType;
       tiling :VkImageTiling;
       usage  :VkImageUsageFlags;
       flags  :VkImageCreateFlags;
     end;

type P_VkQueueFamilyProperties2 = ^VkQueueFamilyProperties2;
     VkQueueFamilyProperties2 = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       queueFamilyProperties :VkQueueFamilyProperties;
     end;

type P_VkPhysicalDeviceMemoryProperties2 = ^VkPhysicalDeviceMemoryProperties2;
     VkPhysicalDeviceMemoryProperties2 = record
       sType            :VkStructureType;
       pNext            :P_void;
       memoryProperties :VkPhysicalDeviceMemoryProperties;
     end;

type P_VkSparseImageFormatProperties2 = ^VkSparseImageFormatProperties2;
     VkSparseImageFormatProperties2 = record
       sType      :VkStructureType;
       pNext      :P_void;
       properties :VkSparseImageFormatProperties;
     end;

type P_VkPhysicalDeviceSparseImageFormatInfo2 = ^VkPhysicalDeviceSparseImageFormatInfo2;
     VkPhysicalDeviceSparseImageFormatInfo2 = record
       sType   :VkStructureType;
       pNext   :P_void;
       format  :VkFormat;
       type_   :VkImageType;
       samples :VkSampleCountFlagBits;
       usage   :VkImageUsageFlags;
       tiling  :VkImageTiling;
     end;

type P_VkPhysicalDevicePointClippingProperties = ^VkPhysicalDevicePointClippingProperties;
     VkPhysicalDevicePointClippingProperties = record
       sType                 :VkStructureType;
       pNext                 :P_void;
       pointClippingBehavior :VkPointClippingBehavior;
     end;

type P_VkInputAttachmentAspectReference = ^VkInputAttachmentAspectReference;
     VkInputAttachmentAspectReference = record
       subpass              :T_uint32_t;
       inputAttachmentIndex :T_uint32_t;
       aspectMask           :VkImageAspectFlags;
     end;

type P_VkRenderPassInputAttachmentAspectCreateInfo = ^VkRenderPassInputAttachmentAspectCreateInfo;
     VkRenderPassInputAttachmentAspectCreateInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       aspectReferenceCount :T_uint32_t;
       pAspectReferences    :P_VkInputAttachmentAspectReference;
     end;

type P_VkImageViewUsageCreateInfo = ^VkImageViewUsageCreateInfo;
     VkImageViewUsageCreateInfo = record
       sType :VkStructureType;
       pNext :P_void;
       usage :VkImageUsageFlags;
     end;

type P_VkPipelineTessellationDomainOriginStateCreateInfo = ^VkPipelineTessellationDomainOriginStateCreateInfo;
     VkPipelineTessellationDomainOriginStateCreateInfo = record
       sType        :VkStructureType;
       pNext        :P_void;
       domainOrigin :VkTessellationDomainOrigin;
     end;

type P_VkRenderPassMultiviewCreateInfo = ^VkRenderPassMultiviewCreateInfo;
     VkRenderPassMultiviewCreateInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       subpassCount         :T_uint32_t;
       pViewMasks           :P_uint32_t;
       dependencyCount      :T_uint32_t;
       pViewOffsets         :P_int32_t;
       correlationMaskCount :T_uint32_t;
       pCorrelationMasks    :P_uint32_t;
     end;

type P_VkPhysicalDeviceMultiviewFeatures = ^VkPhysicalDeviceMultiviewFeatures;
     VkPhysicalDeviceMultiviewFeatures = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       multiview                   :VkBool32;
       multiviewGeometryShader     :VkBool32;
       multiviewTessellationShader :VkBool32;
     end;

type P_VkPhysicalDeviceMultiviewProperties = ^VkPhysicalDeviceMultiviewProperties;
     VkPhysicalDeviceMultiviewProperties = record
       sType                     :VkStructureType;
       pNext                     :P_void;
       maxMultiviewViewCount     :T_uint32_t;
       maxMultiviewInstanceIndex :T_uint32_t;
     end;

type P_VkPhysicalDeviceVariablePointersFeatures = ^VkPhysicalDeviceVariablePointersFeatures;
     VkPhysicalDeviceVariablePointersFeatures = record
       sType                         :VkStructureType;
       pNext                         :P_void;
       variablePointersStorageBuffer :VkBool32;
       variablePointers              :VkBool32;
     end;

type P_VkPhysicalDeviceVariablePointerFeatures = ^VkPhysicalDeviceVariablePointerFeatures;
     VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointersFeatures;

type P_VkPhysicalDeviceProtectedMemoryFeatures = ^VkPhysicalDeviceProtectedMemoryFeatures;
     VkPhysicalDeviceProtectedMemoryFeatures = record
       sType           :VkStructureType;
       pNext           :P_void;
       protectedMemory :VkBool32;
     end;

type P_VkPhysicalDeviceProtectedMemoryProperties = ^VkPhysicalDeviceProtectedMemoryProperties;
     VkPhysicalDeviceProtectedMemoryProperties = record
       sType            :VkStructureType;
       pNext            :P_void;
       protectedNoFault :VkBool32;
     end;

type P_VkDeviceQueueInfo2 = ^VkDeviceQueueInfo2;
     VkDeviceQueueInfo2 = record
       sType            :VkStructureType;
       pNext            :P_void;
       flags            :VkDeviceQueueCreateFlags;
       queueFamilyIndex :T_uint32_t;
       queueIndex       :T_uint32_t;
     end;

type P_VkProtectedSubmitInfo = ^VkProtectedSubmitInfo;
     VkProtectedSubmitInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       protectedSubmit :VkBool32;
     end;

type P_VkSamplerYcbcrConversionCreateInfo = ^VkSamplerYcbcrConversionCreateInfo;
     VkSamplerYcbcrConversionCreateInfo = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       format                      :VkFormat;
       ycbcrModel                  :VkSamplerYcbcrModelConversion;
       ycbcrRange                  :VkSamplerYcbcrRange;
       components                  :VkComponentMapping;
       xChromaOffset               :VkChromaLocation;
       yChromaOffset               :VkChromaLocation;
       chromaFilter                :VkFilter;
       forceExplicitReconstruction :VkBool32;
     end;

type P_VkSamplerYcbcrConversionInfo = ^VkSamplerYcbcrConversionInfo;
     VkSamplerYcbcrConversionInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       conversion :VkSamplerYcbcrConversion;
     end;

type P_VkBindImagePlaneMemoryInfo = ^VkBindImagePlaneMemoryInfo;
     VkBindImagePlaneMemoryInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       planeAspect :VkImageAspectFlagBits;
     end;

type P_VkImagePlaneMemoryRequirementsInfo = ^VkImagePlaneMemoryRequirementsInfo;
     VkImagePlaneMemoryRequirementsInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       planeAspect :VkImageAspectFlagBits;
     end;

type P_VkPhysicalDeviceSamplerYcbcrConversionFeatures = ^VkPhysicalDeviceSamplerYcbcrConversionFeatures;
     VkPhysicalDeviceSamplerYcbcrConversionFeatures = record
       sType                  :VkStructureType;
       pNext                  :P_void;
       samplerYcbcrConversion :VkBool32;
     end;

type P_VkSamplerYcbcrConversionImageFormatProperties = ^VkSamplerYcbcrConversionImageFormatProperties;
     VkSamplerYcbcrConversionImageFormatProperties = record
       sType                               :VkStructureType;
       pNext                               :P_void;
       combinedImageSamplerDescriptorCount :T_uint32_t;
     end;

type P_VkDescriptorUpdateTemplateEntry = ^VkDescriptorUpdateTemplateEntry;
     VkDescriptorUpdateTemplateEntry = record
       dstBinding      :T_uint32_t;
       dstArrayElement :T_uint32_t;
       descriptorCount :T_uint32_t;
       descriptorType  :VkDescriptorType;
       offset          :T_size_t;
       stride          :T_size_t;
     end;

type P_VkDescriptorUpdateTemplateCreateInfo = ^VkDescriptorUpdateTemplateCreateInfo;
     VkDescriptorUpdateTemplateCreateInfo = record
       sType                      :VkStructureType;
       pNext                      :P_void;
       flags                      :VkDescriptorUpdateTemplateCreateFlags;
       descriptorUpdateEntryCount :T_uint32_t;
       pDescriptorUpdateEntries   :P_VkDescriptorUpdateTemplateEntry;
       templateType               :VkDescriptorUpdateTemplateType;
       descriptorSetLayout        :VkDescriptorSetLayout;
       pipelineBindPoint          :VkPipelineBindPoint;
       pipelineLayout             :VkPipelineLayout;
       set_                       :T_uint32_t;
     end;

type P_VkExternalMemoryProperties = ^VkExternalMemoryProperties;
     VkExternalMemoryProperties = record
       externalMemoryFeatures        :VkExternalMemoryFeatureFlags;
       exportFromImportedHandleTypes :VkExternalMemoryHandleTypeFlags;
       compatibleHandleTypes         :VkExternalMemoryHandleTypeFlags;
     end;

type P_VkPhysicalDeviceExternalImageFormatInfo = ^VkPhysicalDeviceExternalImageFormatInfo;
     VkPhysicalDeviceExternalImageFormatInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       handleType :VkExternalMemoryHandleTypeFlagBits;
     end;

type P_VkExternalImageFormatProperties = ^VkExternalImageFormatProperties;
     VkExternalImageFormatProperties = record
       sType                    :VkStructureType;
       pNext                    :P_void;
       externalMemoryProperties :VkExternalMemoryProperties;
     end;

type P_VkPhysicalDeviceExternalBufferInfo = ^VkPhysicalDeviceExternalBufferInfo;
     VkPhysicalDeviceExternalBufferInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       flags      :VkBufferCreateFlags;
       usage      :VkBufferUsageFlags;
       handleType :VkExternalMemoryHandleTypeFlagBits;
     end;

type P_VkExternalBufferProperties = ^VkExternalBufferProperties;
     VkExternalBufferProperties = record
       sType                    :VkStructureType;
       pNext                    :P_void;
       externalMemoryProperties :VkExternalMemoryProperties;
     end;

type P_VkPhysicalDeviceIDProperties = ^VkPhysicalDeviceIDProperties;
     VkPhysicalDeviceIDProperties = record
       sType           :VkStructureType;
       pNext           :P_void;
       deviceUUID      :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
       driverUUID      :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
       deviceLUID      :array [ 0..VK_LUID_SIZE-1 ] of T_uint8_t;
       deviceNodeMask  :T_uint32_t;
       deviceLUIDValid :VkBool32;
     end;

type P_VkExternalMemoryImageCreateInfo = ^VkExternalMemoryImageCreateInfo;
     VkExternalMemoryImageCreateInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       handleTypes :VkExternalMemoryHandleTypeFlags;
     end;

type P_VkExternalMemoryBufferCreateInfo = ^VkExternalMemoryBufferCreateInfo;
     VkExternalMemoryBufferCreateInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       handleTypes :VkExternalMemoryHandleTypeFlags;
     end;

type P_VkExportMemoryAllocateInfo = ^VkExportMemoryAllocateInfo;
     VkExportMemoryAllocateInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       handleTypes :VkExternalMemoryHandleTypeFlags;
     end;

type P_VkPhysicalDeviceExternalFenceInfo = ^VkPhysicalDeviceExternalFenceInfo;
     VkPhysicalDeviceExternalFenceInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       handleType :VkExternalFenceHandleTypeFlagBits;
     end;

type P_VkExternalFenceProperties = ^VkExternalFenceProperties;
     VkExternalFenceProperties = record
       sType                         :VkStructureType;
       pNext                         :P_void;
       exportFromImportedHandleTypes :VkExternalFenceHandleTypeFlags;
       compatibleHandleTypes         :VkExternalFenceHandleTypeFlags;
       externalFenceFeatures         :VkExternalFenceFeatureFlags;
     end;

type P_VkExportFenceCreateInfo = ^VkExportFenceCreateInfo;
     VkExportFenceCreateInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       handleTypes :VkExternalFenceHandleTypeFlags;
     end;

type P_VkExportSemaphoreCreateInfo = ^VkExportSemaphoreCreateInfo;
     VkExportSemaphoreCreateInfo = record
       sType       :VkStructureType;
       pNext       :P_void;
       handleTypes :VkExternalSemaphoreHandleTypeFlags;
     end;

type P_VkPhysicalDeviceExternalSemaphoreInfo = ^VkPhysicalDeviceExternalSemaphoreInfo;
     VkPhysicalDeviceExternalSemaphoreInfo = record
       sType      :VkStructureType;
       pNext      :P_void;
       handleType :VkExternalSemaphoreHandleTypeFlagBits;
     end;

type P_VkExternalSemaphoreProperties = ^VkExternalSemaphoreProperties;
     VkExternalSemaphoreProperties = record
       sType                         :VkStructureType;
       pNext                         :P_void;
       exportFromImportedHandleTypes :VkExternalSemaphoreHandleTypeFlags;
       compatibleHandleTypes         :VkExternalSemaphoreHandleTypeFlags;
       externalSemaphoreFeatures     :VkExternalSemaphoreFeatureFlags;
     end;

type P_VkPhysicalDeviceMaintenance3Properties = ^VkPhysicalDeviceMaintenance3Properties;
     VkPhysicalDeviceMaintenance3Properties = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       maxPerSetDescriptors    :T_uint32_t;
       maxMemoryAllocationSize :VkDeviceSize;
     end;

type P_VkDescriptorSetLayoutSupport = ^VkDescriptorSetLayoutSupport;
     VkDescriptorSetLayoutSupport = record
       sType     :VkStructureType;
       pNext     :P_void;
       supported :VkBool32;
     end;

type P_VkPhysicalDeviceShaderDrawParametersFeatures = ^VkPhysicalDeviceShaderDrawParametersFeatures;
     VkPhysicalDeviceShaderDrawParametersFeatures = record
       sType                :VkStructureType;
       pNext                :P_void;
       shaderDrawParameters :VkBool32;
     end;

type P_VkPhysicalDeviceShaderDrawParameterFeatures = ^VkPhysicalDeviceShaderDrawParameterFeatures;
     VkPhysicalDeviceShaderDrawParameterFeatures = VkPhysicalDeviceShaderDrawParametersFeatures;

type PFN_vkEnumerateInstanceVersion                      = function( pApiVersion_:P_uint32_t ) :VkResult;
type PFN_vkBindBufferMemory2                             = function( device_:VkDevice; bindInfoCount_:T_uint32_t; const pBindInfos_:P_VkBindBufferMemoryInfo ) :VkResult;
type PFN_vkBindImageMemory2                              = function( device_:VkDevice; bindInfoCount_:T_uint32_t; const pBindInfos_:P_VkBindImageMemoryInfo ) :VkResult;
type PFN_vkGetDeviceGroupPeerMemoryFeatures              = procedure( device_:VkDevice; heapIndex_:T_uint32_t; localDeviceIndex_:T_uint32_t; remoteDeviceIndex_:T_uint32_t; pPeerMemoryFeatures_:P_VkPeerMemoryFeatureFlags );
type PFN_vkCmdSetDeviceMask                              = procedure( commandBuffer_:VkCommandBuffer; deviceMask_:T_uint32_t );
type PFN_vkCmdDispatchBase                               = procedure( commandBuffer_:VkCommandBuffer; baseGroupX_:T_uint32_t; baseGroupY_:T_uint32_t; baseGroupZ_:T_uint32_t; groupCountX_:T_uint32_t; groupCountY_:T_uint32_t; groupCountZ_:T_uint32_t );
type PFN_vkEnumeratePhysicalDeviceGroups                 = function( instance_:VkInstance; pPhysicalDeviceGroupCount_:P_uint32_t; pPhysicalDeviceGroupProperties_:P_VkPhysicalDeviceGroupProperties ) :VkResult;
type PFN_vkGetImageMemoryRequirements2                   = procedure( device_:VkDevice; const pInfo_:P_VkImageMemoryRequirementsInfo2; pMemoryRequirements_:P_VkMemoryRequirements2 );
type PFN_vkGetBufferMemoryRequirements2                  = procedure( device_:VkDevice; const pInfo_:P_VkBufferMemoryRequirementsInfo2; pMemoryRequirements_:P_VkMemoryRequirements2 );
type PFN_vkGetImageSparseMemoryRequirements2             = procedure( device_:VkDevice; const pInfo_:P_VkImageSparseMemoryRequirementsInfo2; pSparseMemoryRequirementCount_:P_uint32_t; pSparseMemoryRequirements_:P_VkSparseImageMemoryRequirements2 );
type PFN_vkGetPhysicalDeviceFeatures2                    = procedure( physicalDevice_:VkPhysicalDevice; pFeatures_:P_VkPhysicalDeviceFeatures2 );
type PFN_vkGetPhysicalDeviceProperties2                  = procedure( physicalDevice_:VkPhysicalDevice; pProperties_:P_VkPhysicalDeviceProperties2 );
type PFN_vkGetPhysicalDeviceFormatProperties2            = procedure( physicalDevice_:VkPhysicalDevice; format_:VkFormat; pFormatProperties_:P_VkFormatProperties2 );
type PFN_vkGetPhysicalDeviceImageFormatProperties2       = function( physicalDevice_:VkPhysicalDevice; const pImageFormatInfo_:P_VkPhysicalDeviceImageFormatInfo2; pImageFormatProperties_:P_VkImageFormatProperties2 ) :VkResult;
type PFN_vkGetPhysicalDeviceQueueFamilyProperties2       = procedure( physicalDevice_:VkPhysicalDevice; pQueueFamilyPropertyCount_:P_uint32_t; pQueueFamilyProperties_:P_VkQueueFamilyProperties2 );
type PFN_vkGetPhysicalDeviceMemoryProperties2            = procedure( physicalDevice_:VkPhysicalDevice; pMemoryProperties_:P_VkPhysicalDeviceMemoryProperties2 );
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 = procedure( physicalDevice_:VkPhysicalDevice; const pFormatInfo_:P_VkPhysicalDeviceSparseImageFormatInfo2; pPropertyCount_:P_uint32_t; pProperties_:P_VkSparseImageFormatProperties2 );
type PFN_vkTrimCommandPool                               = procedure( device_:VkDevice; commandPool_:VkCommandPool; flags_:VkCommandPoolTrimFlags );
type PFN_vkGetDeviceQueue2                               = procedure( device_:VkDevice; const pQueueInfo_:P_VkDeviceQueueInfo2; pQueue_:P_VkQueue );
type PFN_vkCreateSamplerYcbcrConversion                  = function( device_:VkDevice; const pCreateInfo_:P_VkSamplerYcbcrConversionCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pYcbcrConversion_:P_VkSamplerYcbcrConversion ) :VkResult;
type PFN_vkDestroySamplerYcbcrConversion                 = procedure( device_:VkDevice; ycbcrConversion_:VkSamplerYcbcrConversion; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCreateDescriptorUpdateTemplate                = function( device_:VkDevice; const pCreateInfo_:P_VkDescriptorUpdateTemplateCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pDescriptorUpdateTemplate_:P_VkDescriptorUpdateTemplate ) :VkResult;
type PFN_vkDestroyDescriptorUpdateTemplate               = procedure( device_:VkDevice; descriptorUpdateTemplate_:VkDescriptorUpdateTemplate; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkUpdateDescriptorSetWithTemplate               = procedure( device_:VkDevice; descriptorSet_:VkDescriptorSet; descriptorUpdateTemplate_:VkDescriptorUpdateTemplate; const pData_:P_void );
type PFN_vkGetPhysicalDeviceExternalBufferProperties     = procedure( physicalDevice_:VkPhysicalDevice; const pExternalBufferInfo_:P_VkPhysicalDeviceExternalBufferInfo; pExternalBufferProperties_:P_VkExternalBufferProperties );
type PFN_vkGetPhysicalDeviceExternalFenceProperties      = procedure( physicalDevice_:VkPhysicalDevice; const pExternalFenceInfo_:P_VkPhysicalDeviceExternalFenceInfo; pExternalFenceProperties_:P_VkExternalFenceProperties );
type PFN_vkGetPhysicalDeviceExternalSemaphoreProperties  = procedure( physicalDevice_:VkPhysicalDevice; const pExternalSemaphoreInfo_:P_VkPhysicalDeviceExternalSemaphoreInfo; pExternalSemaphoreProperties_:P_VkExternalSemaphoreProperties );
type PFN_vkGetDescriptorSetLayoutSupport                 = procedure( device_:VkDevice; const pCreateInfo_:P_VkDescriptorSetLayoutCreateInfo; pSupport_:P_VkDescriptorSetLayoutSupport );

{$IFNDEF VK_NO_PROTOTYPES }
function vkEnumerateInstanceVersion(
    pApiVersion_ :P_uint32_t ) :VkResult; stdcall; external DLLNAME;

function vkBindBufferMemory2(
    device_        :VkDevice;
    bindInfoCount_ :T_uint32_t;
    pBindInfos_    :P_VkBindBufferMemoryInfo ) :VkResult; stdcall; external DLLNAME;

function vkBindImageMemory2(
    device_        :VkDevice;
    bindInfoCount_ :T_uint32_t;
    pBindInfos_    :P_VkBindImageMemoryInfo ) :VkResult; stdcall; external DLLNAME;

procedure vkGetDeviceGroupPeerMemoryFeatures(
    device_              :VkDevice;
    heapIndex_           :T_uint32_t;
    localDeviceIndex_    :T_uint32_t;
    remoteDeviceIndex_   :T_uint32_t;
    pPeerMemoryFeatures_ :P_VkPeerMemoryFeatureFlags ); stdcall; external DLLNAME;

procedure vkCmdSetDeviceMask(
    commandBuffer_ :VkCommandBuffer;
    deviceMask_    :T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDispatchBase(
    commandBuffer_ :VkCommandBuffer;
    baseGroupX_    :T_uint32_t;
    baseGroupY_    :T_uint32_t;
    baseGroupZ_    :T_uint32_t;
    groupCountX_   :T_uint32_t;
    groupCountY_   :T_uint32_t;
    groupCountZ_   :T_uint32_t ); stdcall; external DLLNAME;

function vkEnumeratePhysicalDeviceGroups(
    instance_                       :VkInstance;
    pPhysicalDeviceGroupCount_      :P_uint32_t;
    pPhysicalDeviceGroupProperties_ :P_VkPhysicalDeviceGroupProperties ) :VkResult; stdcall; external DLLNAME;

procedure vkGetImageMemoryRequirements2(
    device_              :VkDevice;
    pInfo_               :P_VkImageMemoryRequirementsInfo2;
    pMemoryRequirements_ :P_VkMemoryRequirements2 ); stdcall; external DLLNAME;

procedure vkGetBufferMemoryRequirements2(
    device_              :VkDevice;
    pInfo_               :P_VkBufferMemoryRequirementsInfo2;
    pMemoryRequirements_ :P_VkMemoryRequirements2 ); stdcall; external DLLNAME;

procedure vkGetImageSparseMemoryRequirements2(
    device_                        :VkDevice;
    pInfo_                         :P_VkImageSparseMemoryRequirementsInfo2;
    pSparseMemoryRequirementCount_ :P_uint32_t;
    pSparseMemoryRequirements_     :P_VkSparseImageMemoryRequirements2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFeatures2(
    physicalDevice_ :VkPhysicalDevice;
    pFeatures_      :P_VkPhysicalDeviceFeatures2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceProperties2(
    physicalDevice_ :VkPhysicalDevice;
    pProperties_    :P_VkPhysicalDeviceProperties2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFormatProperties2(
    physicalDevice_    :VkPhysicalDevice;
    format_            :VkFormat;
    pFormatProperties_ :P_VkFormatProperties2 ); stdcall; external DLLNAME;

function vkGetPhysicalDeviceImageFormatProperties2(
    physicalDevice_         :VkPhysicalDevice;
    pImageFormatInfo_       :P_VkPhysicalDeviceImageFormatInfo2;
    pImageFormatProperties_ :P_VkImageFormatProperties2 ) :VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceQueueFamilyProperties2(
    physicalDevice_            :VkPhysicalDevice;
    pQueueFamilyPropertyCount_ :P_uint32_t;
    pQueueFamilyProperties_    :P_VkQueueFamilyProperties2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceMemoryProperties2(
    physicalDevice_    :VkPhysicalDevice;
    pMemoryProperties_ :P_VkPhysicalDeviceMemoryProperties2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceSparseImageFormatProperties2(
    physicalDevice_ :VkPhysicalDevice;
    pFormatInfo_    :P_VkPhysicalDeviceSparseImageFormatInfo2;
    pPropertyCount_ :P_uint32_t;
    pProperties_    :P_VkSparseImageFormatProperties2 ); stdcall; external DLLNAME;

procedure vkTrimCommandPool(
    device_      :VkDevice;
    commandPool_ :VkCommandPool;
    flags_       :VkCommandPoolTrimFlags ); stdcall; external DLLNAME;

procedure vkGetDeviceQueue2(
    device_     :VkDevice;
    pQueueInfo_ :P_VkDeviceQueueInfo2;
    pQueue_     :P_VkQueue ); stdcall; external DLLNAME;

function vkCreateSamplerYcbcrConversion(
    device_           :VkDevice;
    pCreateInfo_      :P_VkSamplerYcbcrConversionCreateInfo;
    pAllocator_       :P_VkAllocationCallbacks;
    pYcbcrConversion_ :P_VkSamplerYcbcrConversion ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroySamplerYcbcrConversion(
    device_          :VkDevice;
    ycbcrConversion_ :VkSamplerYcbcrConversion;
    pAllocator_      :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateDescriptorUpdateTemplate(
    device_                    :VkDevice;
    pCreateInfo_               :P_VkDescriptorUpdateTemplateCreateInfo;
    pAllocator_                :P_VkAllocationCallbacks;
    pDescriptorUpdateTemplate_ :P_VkDescriptorUpdateTemplate ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDescriptorUpdateTemplate(
    device_                   :VkDevice;
    descriptorUpdateTemplate_ :VkDescriptorUpdateTemplate;
    pAllocator_               :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkUpdateDescriptorSetWithTemplate(
    device_                   :VkDevice;
    descriptorSet_            :VkDescriptorSet;
    descriptorUpdateTemplate_ :VkDescriptorUpdateTemplate;
    pData_                    :P_void ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceExternalBufferProperties(
    physicalDevice_            :VkPhysicalDevice;
    pExternalBufferInfo_       :P_VkPhysicalDeviceExternalBufferInfo;
    pExternalBufferProperties_ :P_VkExternalBufferProperties ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceExternalFenceProperties(
    physicalDevice_           :VkPhysicalDevice;
    pExternalFenceInfo_       :P_VkPhysicalDeviceExternalFenceInfo;
    pExternalFenceProperties_ :P_VkExternalFenceProperties ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceExternalSemaphoreProperties(
    physicalDevice_               :VkPhysicalDevice;
    pExternalSemaphoreInfo_       :P_VkPhysicalDeviceExternalSemaphoreInfo;
    pExternalSemaphoreProperties_ :P_VkExternalSemaphoreProperties ); stdcall; external DLLNAME;

procedure vkGetDescriptorSetLayoutSupport(
    device_      :VkDevice;
    pCreateInfo_ :P_VkDescriptorSetLayoutCreateInfo;
    pSupport_    :P_VkDescriptorSetLayoutSupport ); stdcall; external DLLNAME;
{$ENDIF}


const VK_VERSION_1_2     = 1;
// Vulkan 1.2 version number
const VK_API_VERSION_1_2 = {VK_MAKE_VERSION( 1, 2, 0 )} ( 1 shl 22 ) or ( 2 shl 12 ) or 0; // Patch version should always be set to 0

const VK_MAX_DRIVER_NAME_SIZE = 256;
const VK_MAX_DRIVER_INFO_SIZE = 256;

type P_VkDriverId = ^VkDriverId;
     VkDriverId = (
       VK_DRIVER_ID_AMD_PROPRIETARY               = 1,
       VK_DRIVER_ID_AMD_OPEN_SOURCE               = 2,
       VK_DRIVER_ID_MESA_RADV                     = 3,
       VK_DRIVER_ID_NVIDIA_PROPRIETARY            = 4,
       VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS     = 5,
       VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA        = 6,
       VK_DRIVER_ID_IMAGINATION_PROPRIETARY       = 7,
       VK_DRIVER_ID_QUALCOMM_PROPRIETARY          = 8,
       VK_DRIVER_ID_ARM_PROPRIETARY               = 9,
       VK_DRIVER_ID_GOOGLE_SWIFTSHADER            = 10,
       VK_DRIVER_ID_GGP_PROPRIETARY               = 11,
       VK_DRIVER_ID_BROADCOM_PROPRIETARY          = 12,
       VK_DRIVER_ID_MESA_LLVMPIPE                 = 13,
       VK_DRIVER_ID_MOLTENVK                      = 14,
       VK_DRIVER_ID_AMD_PROPRIETARY_KHR           = VK_DRIVER_ID_AMD_PROPRIETARY,
       VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR           = VK_DRIVER_ID_AMD_OPEN_SOURCE,
       VK_DRIVER_ID_MESA_RADV_KHR                 = VK_DRIVER_ID_MESA_RADV,
       VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR        = VK_DRIVER_ID_NVIDIA_PROPRIETARY,
       VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR = VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS,
       VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR    = VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA,
       VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR   = VK_DRIVER_ID_IMAGINATION_PROPRIETARY,
       VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR      = VK_DRIVER_ID_QUALCOMM_PROPRIETARY,
       VK_DRIVER_ID_ARM_PROPRIETARY_KHR           = VK_DRIVER_ID_ARM_PROPRIETARY,
       VK_DRIVER_ID_GOOGLE_SWIFTSHADER_KHR        = VK_DRIVER_ID_GOOGLE_SWIFTSHADER,
       VK_DRIVER_ID_GGP_PROPRIETARY_KHR           = VK_DRIVER_ID_GGP_PROPRIETARY,
       VK_DRIVER_ID_BROADCOM_PROPRIETARY_KHR      = VK_DRIVER_ID_BROADCOM_PROPRIETARY,
       VK_DRIVER_ID_MAX_ENUM                      = $7FFFFFFF
     );

type P_VkShaderFloatControlsIndependence = ^VkShaderFloatControlsIndependence;
     VkShaderFloatControlsIndependence = (
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY     = 0,
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL             = 1,
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE            = 2,
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR = VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY,
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR         = VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL,
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR        = VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE,
       VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_MAX_ENUM        = $7FFFFFFF
     );

type P_VkSamplerReductionMode = ^VkSamplerReductionMode;
     VkSamplerReductionMode = (
       VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE     = 0,
       VK_SAMPLER_REDUCTION_MODE_MIN                  = 1,
       VK_SAMPLER_REDUCTION_MODE_MAX                  = 2,
       VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE,
       VK_SAMPLER_REDUCTION_MODE_MIN_EXT              = VK_SAMPLER_REDUCTION_MODE_MIN,
       VK_SAMPLER_REDUCTION_MODE_MAX_EXT              = VK_SAMPLER_REDUCTION_MODE_MAX,
       VK_SAMPLER_REDUCTION_MODE_MAX_ENUM             = $7FFFFFFF
     );

type P_VkSemaphoreType = ^VkSemaphoreType;
     VkSemaphoreType = (
       VK_SEMAPHORE_TYPE_BINARY       = 0,
       VK_SEMAPHORE_TYPE_TIMELINE     = 1,
       VK_SEMAPHORE_TYPE_BINARY_KHR   = VK_SEMAPHORE_TYPE_BINARY,
       VK_SEMAPHORE_TYPE_TIMELINE_KHR = VK_SEMAPHORE_TYPE_TIMELINE,
       VK_SEMAPHORE_TYPE_MAX_ENUM     = $7FFFFFFF
     );

type P_VkResolveModeFlagBits = ^VkResolveModeFlagBits;
     VkResolveModeFlagBits = (
       VK_RESOLVE_MODE_NONE                = 0,
       VK_RESOLVE_MODE_SAMPLE_ZERO_BIT     = $00000001,
       VK_RESOLVE_MODE_AVERAGE_BIT         = $00000002,
       VK_RESOLVE_MODE_MIN_BIT             = $00000004,
       VK_RESOLVE_MODE_MAX_BIT             = $00000008,
       VK_RESOLVE_MODE_NONE_KHR            = VK_RESOLVE_MODE_NONE,
       VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = VK_RESOLVE_MODE_SAMPLE_ZERO_BIT,
       VK_RESOLVE_MODE_AVERAGE_BIT_KHR     = VK_RESOLVE_MODE_AVERAGE_BIT,
       VK_RESOLVE_MODE_MIN_BIT_KHR         = VK_RESOLVE_MODE_MIN_BIT,
       VK_RESOLVE_MODE_MAX_BIT_KHR         = VK_RESOLVE_MODE_MAX_BIT,
       VK_RESOLVE_MODE_FLAG_BITS_MAX_ENUM  = $7FFFFFFF
     );
type P_VkResolveModeFlags = ^VkResolveModeFlags;
     VkResolveModeFlags = VkFlags;

type P_VkDescriptorBindingFlagBits = ^VkDescriptorBindingFlagBits;
     VkDescriptorBindingFlagBits = (
       VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT               = $00000001,
       VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT     = $00000002,
       VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT                 = $00000004,
       VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT       = $00000008,
       VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT           = VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT,
       VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT,
       VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT             = VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT,
       VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT   = VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT,
       VK_DESCRIPTOR_BINDING_FLAG_BITS_MAX_ENUM                  = $7FFFFFFF
     );
type P_VkDescriptorBindingFlags = ^VkDescriptorBindingFlags;
     VkDescriptorBindingFlags = VkFlags;

type P_VkSemaphoreWaitFlagBits = ^VkSemaphoreWaitFlagBits;
     VkSemaphoreWaitFlagBits = (
       VK_SEMAPHORE_WAIT_ANY_BIT            = $00000001,
       VK_SEMAPHORE_WAIT_ANY_BIT_KHR        = VK_SEMAPHORE_WAIT_ANY_BIT,
       VK_SEMAPHORE_WAIT_FLAG_BITS_MAX_ENUM = $7FFFFFFF
     );
type P_VkSemaphoreWaitFlags = ^VkSemaphoreWaitFlags;
     VkSemaphoreWaitFlags = VkFlags;
type P_VkPhysicalDeviceVulkan11Features = ^VkPhysicalDeviceVulkan11Features;
     VkPhysicalDeviceVulkan11Features = record
       sType                              :VkStructureType;
       pNext                              :P_void;
       storageBuffer16BitAccess           :VkBool32;
       uniformAndStorageBuffer16BitAccess :VkBool32;
       storagePushConstant16              :VkBool32;
       storageInputOutput16               :VkBool32;
       multiview                          :VkBool32;
       multiviewGeometryShader            :VkBool32;
       multiviewTessellationShader        :VkBool32;
       variablePointersStorageBuffer      :VkBool32;
       variablePointers                   :VkBool32;
       protectedMemory                    :VkBool32;
       samplerYcbcrConversion             :VkBool32;
       shaderDrawParameters               :VkBool32;
     end;

type P_VkPhysicalDeviceVulkan11Properties = ^VkPhysicalDeviceVulkan11Properties;
     VkPhysicalDeviceVulkan11Properties = record
       sType                             :VkStructureType;
       pNext                             :P_void;
       deviceUUID                        :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
       driverUUID                        :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
       deviceLUID                        :array [ 0..VK_LUID_SIZE-1 ] of T_uint8_t;
       deviceNodeMask                    :T_uint32_t;
       deviceLUIDValid                   :VkBool32;
       subgroupSize                      :T_uint32_t;
       subgroupSupportedStages           :VkShaderStageFlags;
       subgroupSupportedOperations       :VkSubgroupFeatureFlags;
       subgroupQuadOperationsInAllStages :VkBool32;
       pointClippingBehavior             :VkPointClippingBehavior;
       maxMultiviewViewCount             :T_uint32_t;
       maxMultiviewInstanceIndex         :T_uint32_t;
       protectedNoFault                  :VkBool32;
       maxPerSetDescriptors              :T_uint32_t;
       maxMemoryAllocationSize           :VkDeviceSize;
     end;

type P_VkPhysicalDeviceVulkan12Features = ^VkPhysicalDeviceVulkan12Features;
     VkPhysicalDeviceVulkan12Features = record
       sType                                              :VkStructureType;
       pNext                                              :P_void;
       samplerMirrorClampToEdge                           :VkBool32;
       drawIndirectCount                                  :VkBool32;
       storageBuffer8BitAccess                            :VkBool32;
       uniformAndStorageBuffer8BitAccess                  :VkBool32;
       storagePushConstant8                               :VkBool32;
       shaderBufferInt64Atomics                           :VkBool32;
       shaderSharedInt64Atomics                           :VkBool32;
       shaderFloat16                                      :VkBool32;
       shaderInt8                                         :VkBool32;
       descriptorIndexing                                 :VkBool32;
       shaderInputAttachmentArrayDynamicIndexing          :VkBool32;
       shaderUniformTexelBufferArrayDynamicIndexing       :VkBool32;
       shaderStorageTexelBufferArrayDynamicIndexing       :VkBool32;
       shaderUniformBufferArrayNonUniformIndexing         :VkBool32;
       shaderSampledImageArrayNonUniformIndexing          :VkBool32;
       shaderStorageBufferArrayNonUniformIndexing         :VkBool32;
       shaderStorageImageArrayNonUniformIndexing          :VkBool32;
       shaderInputAttachmentArrayNonUniformIndexing       :VkBool32;
       shaderUniformTexelBufferArrayNonUniformIndexing    :VkBool32;
       shaderStorageTexelBufferArrayNonUniformIndexing    :VkBool32;
       descriptorBindingUniformBufferUpdateAfterBind      :VkBool32;
       descriptorBindingSampledImageUpdateAfterBind       :VkBool32;
       descriptorBindingStorageImageUpdateAfterBind       :VkBool32;
       descriptorBindingStorageBufferUpdateAfterBind      :VkBool32;
       descriptorBindingUniformTexelBufferUpdateAfterBind :VkBool32;
       descriptorBindingStorageTexelBufferUpdateAfterBind :VkBool32;
       descriptorBindingUpdateUnusedWhilePending          :VkBool32;
       descriptorBindingPartiallyBound                    :VkBool32;
       descriptorBindingVariableDescriptorCount           :VkBool32;
       runtimeDescriptorArray                             :VkBool32;
       samplerFilterMinmax                                :VkBool32;
       scalarBlockLayout                                  :VkBool32;
       imagelessFramebuffer                               :VkBool32;
       uniformBufferStandardLayout                        :VkBool32;
       shaderSubgroupExtendedTypes                        :VkBool32;
       separateDepthStencilLayouts                        :VkBool32;
       hostQueryReset                                     :VkBool32;
       timelineSemaphore                                  :VkBool32;
       bufferDeviceAddress                                :VkBool32;
       bufferDeviceAddressCaptureReplay                   :VkBool32;
       bufferDeviceAddressMultiDevice                     :VkBool32;
       vulkanMemoryModel                                  :VkBool32;
       vulkanMemoryModelDeviceScope                       :VkBool32;
       vulkanMemoryModelAvailabilityVisibilityChains      :VkBool32;
       shaderOutputViewportIndex                          :VkBool32;
       shaderOutputLayer                                  :VkBool32;
       subgroupBroadcastDynamicId                         :VkBool32;
     end;

type P_VkConformanceVersion = ^VkConformanceVersion;
     VkConformanceVersion = record
       major    :T_uint8_t;
       minor    :T_uint8_t;
       subminor :T_uint8_t;
       patch    :T_uint8_t;
     end;

type P_VkPhysicalDeviceVulkan12Properties = ^VkPhysicalDeviceVulkan12Properties;
     VkPhysicalDeviceVulkan12Properties = record
       sType                                                :VkStructureType;
       pNext                                                :P_void;
       driverID                                             :VkDriverId;
       driverName                                           :array [ 0..VK_MAX_DRIVER_NAME_SIZE-1 ] of T_char;
       driverInfo                                           :array [ 0..VK_MAX_DRIVER_INFO_SIZE-1 ] of T_char;
       conformanceVersion                                   :VkConformanceVersion;
       denormBehaviorIndependence                           :VkShaderFloatControlsIndependence;
       roundingModeIndependence                             :VkShaderFloatControlsIndependence;
       shaderSignedZeroInfNanPreserveFloat16                :VkBool32;
       shaderSignedZeroInfNanPreserveFloat32                :VkBool32;
       shaderSignedZeroInfNanPreserveFloat64                :VkBool32;
       shaderDenormPreserveFloat16                          :VkBool32;
       shaderDenormPreserveFloat32                          :VkBool32;
       shaderDenormPreserveFloat64                          :VkBool32;
       shaderDenormFlushToZeroFloat16                       :VkBool32;
       shaderDenormFlushToZeroFloat32                       :VkBool32;
       shaderDenormFlushToZeroFloat64                       :VkBool32;
       shaderRoundingModeRTEFloat16                         :VkBool32;
       shaderRoundingModeRTEFloat32                         :VkBool32;
       shaderRoundingModeRTEFloat64                         :VkBool32;
       shaderRoundingModeRTZFloat16                         :VkBool32;
       shaderRoundingModeRTZFloat32                         :VkBool32;
       shaderRoundingModeRTZFloat64                         :VkBool32;
       maxUpdateAfterBindDescriptorsInAllPools              :T_uint32_t;
       shaderUniformBufferArrayNonUniformIndexingNative     :VkBool32;
       shaderSampledImageArrayNonUniformIndexingNative      :VkBool32;
       shaderStorageBufferArrayNonUniformIndexingNative     :VkBool32;
       shaderStorageImageArrayNonUniformIndexingNative      :VkBool32;
       shaderInputAttachmentArrayNonUniformIndexingNative   :VkBool32;
       robustBufferAccessUpdateAfterBind                    :VkBool32;
       quadDivergentImplicitLod                             :VkBool32;
       maxPerStageDescriptorUpdateAfterBindSamplers         :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindUniformBuffers   :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindStorageBuffers   :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindSampledImages    :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindStorageImages    :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindInputAttachments :T_uint32_t;
       maxPerStageUpdateAfterBindResources                  :T_uint32_t;
       maxDescriptorSetUpdateAfterBindSamplers              :T_uint32_t;
       maxDescriptorSetUpdateAfterBindUniformBuffers        :T_uint32_t;
       maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :T_uint32_t;
       maxDescriptorSetUpdateAfterBindStorageBuffers        :T_uint32_t;
       maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :T_uint32_t;
       maxDescriptorSetUpdateAfterBindSampledImages         :T_uint32_t;
       maxDescriptorSetUpdateAfterBindStorageImages         :T_uint32_t;
       maxDescriptorSetUpdateAfterBindInputAttachments      :T_uint32_t;
       supportedDepthResolveModes                           :VkResolveModeFlags;
       supportedStencilResolveModes                         :VkResolveModeFlags;
       independentResolveNone                               :VkBool32;
       independentResolve                                   :VkBool32;
       filterMinmaxSingleComponentFormats                   :VkBool32;
       filterMinmaxImageComponentMapping                    :VkBool32;
       maxTimelineSemaphoreValueDifference                  :T_uint64_t;
       framebufferIntegerColorSampleCounts                  :VkSampleCountFlags;
     end;

type P_VkImageFormatListCreateInfo = ^VkImageFormatListCreateInfo;
     VkImageFormatListCreateInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       viewFormatCount :T_uint32_t;
       pViewFormats    :P_VkFormat;
     end;

type P_VkAttachmentDescription2 = ^VkAttachmentDescription2;
     VkAttachmentDescription2 = record
       sType          :VkStructureType;
       pNext          :P_void;
       flags          :VkAttachmentDescriptionFlags;
       format         :VkFormat;
       samples        :VkSampleCountFlagBits;
       loadOp         :VkAttachmentLoadOp;
       storeOp        :VkAttachmentStoreOp;
       stencilLoadOp  :VkAttachmentLoadOp;
       stencilStoreOp :VkAttachmentStoreOp;
       initialLayout  :VkImageLayout;
       finalLayout    :VkImageLayout;
     end;

type P_VkAttachmentReference2 = ^VkAttachmentReference2;
     VkAttachmentReference2 = record
       sType      :VkStructureType;
       pNext      :P_void;
       attachment :T_uint32_t;
       layout     :VkImageLayout;
       aspectMask :VkImageAspectFlags;
     end;

type P_VkSubpassDescription2 = ^VkSubpassDescription2;
     VkSubpassDescription2 = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       flags                   :VkSubpassDescriptionFlags;
       pipelineBindPoint       :VkPipelineBindPoint;
       viewMask                :T_uint32_t;
       inputAttachmentCount    :T_uint32_t;
       pInputAttachments       :P_VkAttachmentReference2;
       colorAttachmentCount    :T_uint32_t;
       pColorAttachments       :P_VkAttachmentReference2;
       pResolveAttachments     :P_VkAttachmentReference2;
       pDepthStencilAttachment :P_VkAttachmentReference2;
       preserveAttachmentCount :T_uint32_t;
       pPreserveAttachments    :P_uint32_t;
     end;

type P_VkSubpassDependency2 = ^VkSubpassDependency2;
     VkSubpassDependency2 = record
       sType           :VkStructureType;
       pNext           :P_void;
       srcSubpass      :T_uint32_t;
       dstSubpass      :T_uint32_t;
       srcStageMask    :VkPipelineStageFlags;
       dstStageMask    :VkPipelineStageFlags;
       srcAccessMask   :VkAccessFlags;
       dstAccessMask   :VkAccessFlags;
       dependencyFlags :VkDependencyFlags;
       viewOffset      :T_int32_t;
     end;

type P_VkRenderPassCreateInfo2 = ^VkRenderPassCreateInfo2;
     VkRenderPassCreateInfo2 = record
       sType                   :VkStructureType;
       pNext                   :P_void;
       flags                   :VkRenderPassCreateFlags;
       attachmentCount         :T_uint32_t;
       pAttachments            :P_VkAttachmentDescription2;
       subpassCount            :T_uint32_t;
       pSubpasses              :P_VkSubpassDescription2;
       dependencyCount         :T_uint32_t;
       pDependencies           :P_VkSubpassDependency2;
       correlatedViewMaskCount :T_uint32_t;
       pCorrelatedViewMasks    :P_uint32_t;
     end;

type P_VkSubpassBeginInfo = ^VkSubpassBeginInfo;
     VkSubpassBeginInfo = record
       sType    :VkStructureType;
       pNext    :P_void;
       contents :VkSubpassContents;
     end;

type P_VkSubpassEndInfo = ^VkSubpassEndInfo;
     VkSubpassEndInfo = record
       sType :VkStructureType;
       pNext :P_void;
     end;

type P_VkPhysicalDevice8BitStorageFeatures = ^VkPhysicalDevice8BitStorageFeatures;
     VkPhysicalDevice8BitStorageFeatures = record
       sType                             :VkStructureType;
       pNext                             :P_void;
       storageBuffer8BitAccess           :VkBool32;
       uniformAndStorageBuffer8BitAccess :VkBool32;
       storagePushConstant8              :VkBool32;
     end;

type P_VkPhysicalDeviceDriverProperties = ^VkPhysicalDeviceDriverProperties;
     VkPhysicalDeviceDriverProperties = record
       sType              :VkStructureType;
       pNext              :P_void;
       driverID           :VkDriverId;
       driverName         :array [ 0..VK_MAX_DRIVER_NAME_SIZE-1 ] of T_char;
       driverInfo         :array [ 0..VK_MAX_DRIVER_INFO_SIZE-1 ] of T_char;
       conformanceVersion :VkConformanceVersion;
     end;

type P_VkPhysicalDeviceShaderAtomicInt64Features = ^VkPhysicalDeviceShaderAtomicInt64Features;
     VkPhysicalDeviceShaderAtomicInt64Features = record
       sType                    :VkStructureType;
       pNext                    :P_void;
       shaderBufferInt64Atomics :VkBool32;
       shaderSharedInt64Atomics :VkBool32;
     end;

type P_VkPhysicalDeviceShaderFloat16Int8Features = ^VkPhysicalDeviceShaderFloat16Int8Features;
     VkPhysicalDeviceShaderFloat16Int8Features = record
       sType         :VkStructureType;
       pNext         :P_void;
       shaderFloat16 :VkBool32;
       shaderInt8    :VkBool32;
     end;

type P_VkPhysicalDeviceFloatControlsProperties = ^VkPhysicalDeviceFloatControlsProperties;
     VkPhysicalDeviceFloatControlsProperties = record
       sType                                 :VkStructureType;
       pNext                                 :P_void;
       denormBehaviorIndependence            :VkShaderFloatControlsIndependence;
       roundingModeIndependence              :VkShaderFloatControlsIndependence;
       shaderSignedZeroInfNanPreserveFloat16 :VkBool32;
       shaderSignedZeroInfNanPreserveFloat32 :VkBool32;
       shaderSignedZeroInfNanPreserveFloat64 :VkBool32;
       shaderDenormPreserveFloat16           :VkBool32;
       shaderDenormPreserveFloat32           :VkBool32;
       shaderDenormPreserveFloat64           :VkBool32;
       shaderDenormFlushToZeroFloat16        :VkBool32;
       shaderDenormFlushToZeroFloat32        :VkBool32;
       shaderDenormFlushToZeroFloat64        :VkBool32;
       shaderRoundingModeRTEFloat16          :VkBool32;
       shaderRoundingModeRTEFloat32          :VkBool32;
       shaderRoundingModeRTEFloat64          :VkBool32;
       shaderRoundingModeRTZFloat16          :VkBool32;
       shaderRoundingModeRTZFloat32          :VkBool32;
       shaderRoundingModeRTZFloat64          :VkBool32;
     end;

type P_VkDescriptorSetLayoutBindingFlagsCreateInfo = ^VkDescriptorSetLayoutBindingFlagsCreateInfo;
     VkDescriptorSetLayoutBindingFlagsCreateInfo = record
       sType         :VkStructureType;
       pNext         :P_void;
       bindingCount  :T_uint32_t;
       pBindingFlags :P_VkDescriptorBindingFlags;
     end;

type P_VkPhysicalDeviceDescriptorIndexingFeatures = ^VkPhysicalDeviceDescriptorIndexingFeatures;
     VkPhysicalDeviceDescriptorIndexingFeatures = record
       sType                                              :VkStructureType;
       pNext                                              :P_void;
       shaderInputAttachmentArrayDynamicIndexing          :VkBool32;
       shaderUniformTexelBufferArrayDynamicIndexing       :VkBool32;
       shaderStorageTexelBufferArrayDynamicIndexing       :VkBool32;
       shaderUniformBufferArrayNonUniformIndexing         :VkBool32;
       shaderSampledImageArrayNonUniformIndexing          :VkBool32;
       shaderStorageBufferArrayNonUniformIndexing         :VkBool32;
       shaderStorageImageArrayNonUniformIndexing          :VkBool32;
       shaderInputAttachmentArrayNonUniformIndexing       :VkBool32;
       shaderUniformTexelBufferArrayNonUniformIndexing    :VkBool32;
       shaderStorageTexelBufferArrayNonUniformIndexing    :VkBool32;
       descriptorBindingUniformBufferUpdateAfterBind      :VkBool32;
       descriptorBindingSampledImageUpdateAfterBind       :VkBool32;
       descriptorBindingStorageImageUpdateAfterBind       :VkBool32;
       descriptorBindingStorageBufferUpdateAfterBind      :VkBool32;
       descriptorBindingUniformTexelBufferUpdateAfterBind :VkBool32;
       descriptorBindingStorageTexelBufferUpdateAfterBind :VkBool32;
       descriptorBindingUpdateUnusedWhilePending          :VkBool32;
       descriptorBindingPartiallyBound                    :VkBool32;
       descriptorBindingVariableDescriptorCount           :VkBool32;
       runtimeDescriptorArray                             :VkBool32;
     end;

type P_VkPhysicalDeviceDescriptorIndexingProperties = ^VkPhysicalDeviceDescriptorIndexingProperties;
     VkPhysicalDeviceDescriptorIndexingProperties = record
       sType                                                :VkStructureType;
       pNext                                                :P_void;
       maxUpdateAfterBindDescriptorsInAllPools              :T_uint32_t;
       shaderUniformBufferArrayNonUniformIndexingNative     :VkBool32;
       shaderSampledImageArrayNonUniformIndexingNative      :VkBool32;
       shaderStorageBufferArrayNonUniformIndexingNative     :VkBool32;
       shaderStorageImageArrayNonUniformIndexingNative      :VkBool32;
       shaderInputAttachmentArrayNonUniformIndexingNative   :VkBool32;
       robustBufferAccessUpdateAfterBind                    :VkBool32;
       quadDivergentImplicitLod                             :VkBool32;
       maxPerStageDescriptorUpdateAfterBindSamplers         :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindUniformBuffers   :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindStorageBuffers   :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindSampledImages    :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindStorageImages    :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindInputAttachments :T_uint32_t;
       maxPerStageUpdateAfterBindResources                  :T_uint32_t;
       maxDescriptorSetUpdateAfterBindSamplers              :T_uint32_t;
       maxDescriptorSetUpdateAfterBindUniformBuffers        :T_uint32_t;
       maxDescriptorSetUpdateAfterBindUniformBuffersDynamic :T_uint32_t;
       maxDescriptorSetUpdateAfterBindStorageBuffers        :T_uint32_t;
       maxDescriptorSetUpdateAfterBindStorageBuffersDynamic :T_uint32_t;
       maxDescriptorSetUpdateAfterBindSampledImages         :T_uint32_t;
       maxDescriptorSetUpdateAfterBindStorageImages         :T_uint32_t;
       maxDescriptorSetUpdateAfterBindInputAttachments      :T_uint32_t;
     end;

type P_VkDescriptorSetVariableDescriptorCountAllocateInfo = ^VkDescriptorSetVariableDescriptorCountAllocateInfo;
     VkDescriptorSetVariableDescriptorCountAllocateInfo = record
       sType              :VkStructureType;
       pNext              :P_void;
       descriptorSetCount :T_uint32_t;
       pDescriptorCounts  :P_uint32_t;
     end;

type P_VkDescriptorSetVariableDescriptorCountLayoutSupport = ^VkDescriptorSetVariableDescriptorCountLayoutSupport;
     VkDescriptorSetVariableDescriptorCountLayoutSupport = record
       sType                      :VkStructureType;
       pNext                      :P_void;
       maxVariableDescriptorCount :T_uint32_t;
     end;

type P_VkSubpassDescriptionDepthStencilResolve = ^VkSubpassDescriptionDepthStencilResolve;
     VkSubpassDescriptionDepthStencilResolve = record
       sType                          :VkStructureType;
       pNext                          :P_void;
       depthResolveMode               :VkResolveModeFlagBits;
       stencilResolveMode             :VkResolveModeFlagBits;
       pDepthStencilResolveAttachment :P_VkAttachmentReference2;
     end;

type P_VkPhysicalDeviceDepthStencilResolveProperties = ^VkPhysicalDeviceDepthStencilResolveProperties;
     VkPhysicalDeviceDepthStencilResolveProperties = record
       sType                        :VkStructureType;
       pNext                        :P_void;
       supportedDepthResolveModes   :VkResolveModeFlags;
       supportedStencilResolveModes :VkResolveModeFlags;
       independentResolveNone       :VkBool32;
       independentResolve           :VkBool32;
     end;

type P_VkPhysicalDeviceScalarBlockLayoutFeatures = ^VkPhysicalDeviceScalarBlockLayoutFeatures;
     VkPhysicalDeviceScalarBlockLayoutFeatures = record
       sType             :VkStructureType;
       pNext             :P_void;
       scalarBlockLayout :VkBool32;
     end;

type P_VkImageStencilUsageCreateInfo = ^VkImageStencilUsageCreateInfo;
     VkImageStencilUsageCreateInfo = record
       sType        :VkStructureType;
       pNext        :P_void;
       stencilUsage :VkImageUsageFlags;
     end;

type P_VkSamplerReductionModeCreateInfo = ^VkSamplerReductionModeCreateInfo;
     VkSamplerReductionModeCreateInfo = record
       sType         :VkStructureType;
       pNext         :P_void;
       reductionMode :VkSamplerReductionMode;
     end;

type P_VkPhysicalDeviceSamplerFilterMinmaxProperties = ^VkPhysicalDeviceSamplerFilterMinmaxProperties;
     VkPhysicalDeviceSamplerFilterMinmaxProperties = record
       sType                              :VkStructureType;
       pNext                              :P_void;
       filterMinmaxSingleComponentFormats :VkBool32;
       filterMinmaxImageComponentMapping  :VkBool32;
     end;

type P_VkPhysicalDeviceVulkanMemoryModelFeatures = ^VkPhysicalDeviceVulkanMemoryModelFeatures;
     VkPhysicalDeviceVulkanMemoryModelFeatures = record
       sType                                         :VkStructureType;
       pNext                                         :P_void;
       vulkanMemoryModel                             :VkBool32;
       vulkanMemoryModelDeviceScope                  :VkBool32;
       vulkanMemoryModelAvailabilityVisibilityChains :VkBool32;
     end;

type P_VkPhysicalDeviceImagelessFramebufferFeatures = ^VkPhysicalDeviceImagelessFramebufferFeatures;
     VkPhysicalDeviceImagelessFramebufferFeatures = record
       sType                :VkStructureType;
       pNext                :P_void;
       imagelessFramebuffer :VkBool32;
     end;

type P_VkFramebufferAttachmentImageInfo = ^VkFramebufferAttachmentImageInfo;
     VkFramebufferAttachmentImageInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       flags           :VkImageCreateFlags;
       usage           :VkImageUsageFlags;
       width           :T_uint32_t;
       height          :T_uint32_t;
       layerCount      :T_uint32_t;
       viewFormatCount :T_uint32_t;
       pViewFormats    :P_VkFormat;
     end;

type P_VkFramebufferAttachmentsCreateInfo = ^VkFramebufferAttachmentsCreateInfo;
     VkFramebufferAttachmentsCreateInfo = record
       sType                    :VkStructureType;
       pNext                    :P_void;
       attachmentImageInfoCount :T_uint32_t;
       pAttachmentImageInfos    :P_VkFramebufferAttachmentImageInfo;
     end;

type P_VkRenderPassAttachmentBeginInfo = ^VkRenderPassAttachmentBeginInfo;
     VkRenderPassAttachmentBeginInfo = record
       sType           :VkStructureType;
       pNext           :P_void;
       attachmentCount :T_uint32_t;
       pAttachments    :P_VkImageView;
     end;

type P_VkPhysicalDeviceUniformBufferStandardLayoutFeatures = ^VkPhysicalDeviceUniformBufferStandardLayoutFeatures;
     VkPhysicalDeviceUniformBufferStandardLayoutFeatures = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       uniformBufferStandardLayout :VkBool32;
     end;

type P_VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures = ^VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures;
     VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       shaderSubgroupExtendedTypes :VkBool32;
     end;

type P_VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures = ^VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures;
     VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       separateDepthStencilLayouts :VkBool32;
     end;

type P_VkAttachmentReferenceStencilLayout = ^VkAttachmentReferenceStencilLayout;
     VkAttachmentReferenceStencilLayout = record
       sType         :VkStructureType;
       pNext         :P_void;
       stencilLayout :VkImageLayout;
     end;

type P_VkAttachmentDescriptionStencilLayout = ^VkAttachmentDescriptionStencilLayout;
     VkAttachmentDescriptionStencilLayout = record
       sType                :VkStructureType;
       pNext                :P_void;
       stencilInitialLayout :VkImageLayout;
       stencilFinalLayout   :VkImageLayout;
     end;

type P_VkPhysicalDeviceHostQueryResetFeatures = ^VkPhysicalDeviceHostQueryResetFeatures;
     VkPhysicalDeviceHostQueryResetFeatures = record
       sType          :VkStructureType;
       pNext          :P_void;
       hostQueryReset :VkBool32;
     end;

type P_VkPhysicalDeviceTimelineSemaphoreFeatures = ^VkPhysicalDeviceTimelineSemaphoreFeatures;
     VkPhysicalDeviceTimelineSemaphoreFeatures = record
       sType             :VkStructureType;
       pNext             :P_void;
       timelineSemaphore :VkBool32;
     end;

type P_VkPhysicalDeviceTimelineSemaphoreProperties = ^VkPhysicalDeviceTimelineSemaphoreProperties;
     VkPhysicalDeviceTimelineSemaphoreProperties = record
       sType                               :VkStructureType;
       pNext                               :P_void;
       maxTimelineSemaphoreValueDifference :T_uint64_t;
     end;

type P_VkSemaphoreTypeCreateInfo = ^VkSemaphoreTypeCreateInfo;
     VkSemaphoreTypeCreateInfo = record
       sType         :VkStructureType;
       pNext         :P_void;
       semaphoreType :VkSemaphoreType;
       initialValue  :T_uint64_t;
     end;

type P_VkTimelineSemaphoreSubmitInfo = ^VkTimelineSemaphoreSubmitInfo;
     VkTimelineSemaphoreSubmitInfo = record
       sType                     :VkStructureType;
       pNext                     :P_void;
       waitSemaphoreValueCount   :T_uint32_t;
       pWaitSemaphoreValues      :P_uint64_t;
       signalSemaphoreValueCount :T_uint32_t;
       pSignalSemaphoreValues    :P_uint64_t;
     end;

type P_VkSemaphoreWaitInfo = ^VkSemaphoreWaitInfo;
     VkSemaphoreWaitInfo = record
       sType          :VkStructureType;
       pNext          :P_void;
       flags          :VkSemaphoreWaitFlags;
       semaphoreCount :T_uint32_t;
       pSemaphores    :P_VkSemaphore;
       pValues        :P_uint64_t;
     end;

type P_VkSemaphoreSignalInfo = ^VkSemaphoreSignalInfo;
     VkSemaphoreSignalInfo = record
       sType     :VkStructureType;
       pNext     :P_void;
       semaphore :VkSemaphore;
       value     :T_uint64_t;
     end;

type P_VkPhysicalDeviceBufferDeviceAddressFeatures = ^VkPhysicalDeviceBufferDeviceAddressFeatures;
     VkPhysicalDeviceBufferDeviceAddressFeatures = record
       sType                            :VkStructureType;
       pNext                            :P_void;
       bufferDeviceAddress              :VkBool32;
       bufferDeviceAddressCaptureReplay :VkBool32;
       bufferDeviceAddressMultiDevice   :VkBool32;
     end;

type P_VkBufferDeviceAddressInfo = ^VkBufferDeviceAddressInfo;
     VkBufferDeviceAddressInfo = record
       sType  :VkStructureType;
       pNext  :P_void;
       buffer :VkBuffer;
     end;

type P_VkBufferOpaqueCaptureAddressCreateInfo = ^VkBufferOpaqueCaptureAddressCreateInfo;
     VkBufferOpaqueCaptureAddressCreateInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       opaqueCaptureAddress :T_uint64_t;
     end;

type P_VkMemoryOpaqueCaptureAddressAllocateInfo = ^VkMemoryOpaqueCaptureAddressAllocateInfo;
     VkMemoryOpaqueCaptureAddressAllocateInfo = record
       sType                :VkStructureType;
       pNext                :P_void;
       opaqueCaptureAddress :T_uint64_t;
     end;

type P_VkDeviceMemoryOpaqueCaptureAddressInfo = ^VkDeviceMemoryOpaqueCaptureAddressInfo;
     VkDeviceMemoryOpaqueCaptureAddressInfo = record
       sType  :VkStructureType;
       pNext  :P_void;
       memory :VkDeviceMemory;
     end;

type PFN_vkCmdDrawIndirectCount                = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCmdDrawIndexedIndirectCount         = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCreateRenderPass2                   = function( device_:VkDevice; const pCreateInfo_:P_VkRenderPassCreateInfo2; const pAllocator_:P_VkAllocationCallbacks; pRenderPass_:P_VkRenderPass ) :VkResult;
type PFN_vkCmdBeginRenderPass2                 = procedure( commandBuffer_:VkCommandBuffer; const pRenderPassBegin_:P_VkRenderPassBeginInfo; const pSubpassBeginInfo_:P_VkSubpassBeginInfo );
type PFN_vkCmdNextSubpass2                     = procedure( commandBuffer_:VkCommandBuffer; const pSubpassBeginInfo_:P_VkSubpassBeginInfo; const pSubpassEndInfo_:P_VkSubpassEndInfo );
type PFN_vkCmdEndRenderPass2                   = procedure( commandBuffer_:VkCommandBuffer; const pSubpassEndInfo_:P_VkSubpassEndInfo );
type PFN_vkResetQueryPool                      = procedure( device_:VkDevice; queryPool_:VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t );
type PFN_vkGetSemaphoreCounterValue            = function( device_:VkDevice; semaphore_:VkSemaphore; pValue_:P_uint64_t ) :VkResult;
type PFN_vkWaitSemaphores                      = function( device_:VkDevice; const pWaitInfo_:P_VkSemaphoreWaitInfo; timeout_:T_uint64_t ) :VkResult;
type PFN_vkSignalSemaphore                     = function( device_:VkDevice; const pSignalInfo_:P_VkSemaphoreSignalInfo ) :VkResult;
type PFN_vkGetBufferDeviceAddress              = function( device_:VkDevice; const pInfo_:P_VkBufferDeviceAddressInfo ) :VkDeviceAddress;
type PFN_vkGetBufferOpaqueCaptureAddress       = function( device_:VkDevice; const pInfo_:P_VkBufferDeviceAddressInfo ) :T_uint64_t;
type PFN_vkGetDeviceMemoryOpaqueCaptureAddress = function( device_:VkDevice; const pInfo_:P_VkDeviceMemoryOpaqueCaptureAddressInfo ) :T_uint64_t;

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdDrawIndirectCount(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexedIndirectCount(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;

function vkCreateRenderPass2(
    device_:VkDevice;
    pCreateInfo_:P_VkRenderPassCreateInfo2;
    pAllocator_:P_VkAllocationCallbacks;
    pRenderPass_:P_VkRenderPass ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdBeginRenderPass2(
    commandBuffer_:VkCommandBuffer;
    pRenderPassBegin_:P_VkRenderPassBeginInfo;
    pSubpassBeginInfo_:P_VkSubpassBeginInfo ); stdcall; external DLLNAME;

procedure vkCmdNextSubpass2(
    commandBuffer_:VkCommandBuffer;
    pSubpassBeginInfo_:P_VkSubpassBeginInfo;
    pSubpassEndInfo_:P_VkSubpassEndInfo ); stdcall; external DLLNAME;

procedure vkCmdEndRenderPass2(
    commandBuffer_:VkCommandBuffer;
    pSubpassEndInfo_:P_VkSubpassEndInfo ); stdcall; external DLLNAME;

procedure vkResetQueryPool(
    device_:VkDevice;
    queryPool_:VkQueryPool;
    firstQuery_:T_uint32_t;
    queryCount_:T_uint32_t ); stdcall; external DLLNAME;

function vkGetSemaphoreCounterValue(
    device_:VkDevice;
    semaphore_:VkSemaphore;
    pValue_:P_uint64_t ) :VkResult; stdcall; external DLLNAME;

function vkWaitSemaphores(
    device_:VkDevice;
    pWaitInfo_:P_VkSemaphoreWaitInfo;
    timeout_:T_uint64_t ) :VkResult; stdcall; external DLLNAME;

function vkSignalSemaphore(
    device_:VkDevice;
    pSignalInfo_:P_VkSemaphoreSignalInfo ) :VkResult; stdcall; external DLLNAME;

function vkGetBufferDeviceAddress(
    device_:VkDevice;
    pInfo_:P_VkBufferDeviceAddressInfo ) :VkDeviceAddress; stdcall; external DLLNAME;

function vkGetBufferOpaqueCaptureAddress(
    device_:VkDevice;
    pInfo_:P_VkBufferDeviceAddressInfo ) :T_uint64_t; stdcall; external DLLNAME;

function vkGetDeviceMemoryOpaqueCaptureAddress(
    device_:VkDevice;
    pInfo_:P_VkDeviceMemoryOpaqueCaptureAddressInfo ) :T_uint64_t; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_surface = 1;
type P_VkSurfaceKHR = ^VkSurfaceKHR;
     VkSurfaceKHR = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_KHR_SURFACE_SPEC_VERSION       = 25;
const VK_KHR_SURFACE_EXTENSION_NAME = 'VK_KHR_surface';

type P_VkPresentModeKHR = ^VkPresentModeKHR;
     VkPresentModeKHR = (
       VK_PRESENT_MODE_IMMEDIATE_KHR = 0,
       VK_PRESENT_MODE_MAILBOX_KHR = 1,
       VK_PRESENT_MODE_FIFO_KHR = 2,
       VK_PRESENT_MODE_FIFO_RELAXED_KHR = 3,
       VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = 1000111000,
       VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = 1000111001,
       VK_PRESENT_MODE_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkColorSpaceKHR = ^VkColorSpaceKHR;
     VkColorSpaceKHR = (
       VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = 0,
       VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT = 1000104001,
       VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT = 1000104002,
       VK_COLOR_SPACE_DISPLAY_P3_LINEAR_EXT = 1000104003,
       VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT = 1000104004,
       VK_COLOR_SPACE_BT709_LINEAR_EXT = 1000104005,
       VK_COLOR_SPACE_BT709_NONLINEAR_EXT = 1000104006,
       VK_COLOR_SPACE_BT2020_LINEAR_EXT = 1000104007,
       VK_COLOR_SPACE_HDR10_ST2084_EXT = 1000104008,
       VK_COLOR_SPACE_DOLBYVISION_EXT = 1000104009,
       VK_COLOR_SPACE_HDR10_HLG_EXT = 1000104010,
       VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT = 1000104011,
       VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT = 1000104012,
       VK_COLOR_SPACE_PASS_THROUGH_EXT = 1000104013,
       VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = 1000104014,
       VK_COLOR_SPACE_DISPLAY_NATIVE_AMD = 1000213000,
       VK_COLORSPACE_SRGB_NONLINEAR_KHR = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
       VK_COLOR_SPACE_DCI_P3_LINEAR_EXT = VK_COLOR_SPACE_DISPLAY_P3_LINEAR_EXT,
       VK_COLOR_SPACE_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkSurfaceTransformFlagBitsKHR = ^VkSurfaceTransformFlagBitsKHR;
     VkSurfaceTransformFlagBitsKHR = (
       VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = $00000001,
       VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = $00000002,
       VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = $00000004,
       VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = $00000008,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = $00000010,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = $00000020,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = $00000040,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = $00000080,
       VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = $00000100,
       VK_SURFACE_TRANSFORM_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkCompositeAlphaFlagBitsKHR = ^VkCompositeAlphaFlagBitsKHR;
     VkCompositeAlphaFlagBitsKHR = (
       VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = $00000001,
       VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = $00000002,
       VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = $00000004,
       VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = $00000008,
       VK_COMPOSITE_ALPHA_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkCompositeAlphaFlagsKHR = ^VkCompositeAlphaFlagsKHR;
     VkCompositeAlphaFlagsKHR = VkFlags;
type P_VkSurfaceTransformFlagsKHR = ^VkSurfaceTransformFlagsKHR;
     VkSurfaceTransformFlagsKHR = VkFlags;
type P_VkSurfaceCapabilitiesKHR = ^VkSurfaceCapabilitiesKHR;
     VkSurfaceCapabilitiesKHR = record
       minImageCount :T_uint32_t;
       maxImageCount :T_uint32_t;
       currentExtent :VkExtent2D;
       minImageExtent :VkExtent2D;
       maxImageExtent :VkExtent2D;
       maxImageArrayLayers :T_uint32_t;
       supportedTransforms :VkSurfaceTransformFlagsKHR;
       currentTransform :VkSurfaceTransformFlagBitsKHR;
       supportedCompositeAlpha :VkCompositeAlphaFlagsKHR;
       supportedUsageFlags :VkImageUsageFlags;
     end;

type P_VkSurfaceFormatKHR = ^VkSurfaceFormatKHR;
     VkSurfaceFormatKHR = record
       format :VkFormat;
       colorSpace :VkColorSpaceKHR;
     end;

type PFN_vkDestroySurfaceKHR = procedure( instance_:VkInstance; surface_:VkSurfaceKHR; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetPhysicalDeviceSurfaceSupportKHR = function( physicalDevice_:VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; surface_:VkSurfaceKHR; pSupported_:P_VkBool32 ) :VkResult;
type PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR = function( physicalDevice_:VkPhysicalDevice; surface_:VkSurfaceKHR; pSurfaceCapabilities_:P_VkSurfaceCapabilitiesKHR ) :VkResult;
type PFN_vkGetPhysicalDeviceSurfaceFormatsKHR = function( physicalDevice_:VkPhysicalDevice; surface_:VkSurfaceKHR; pSurfaceFormatCount_:P_uint32_t; pSurfaceFormats_:P_VkSurfaceFormatKHR ) :VkResult;
type PFN_vkGetPhysicalDeviceSurfacePresentModesKHR = function( physicalDevice_:VkPhysicalDevice; surface_:VkSurfaceKHR; pPresentModeCount_:P_uint32_t; pPresentModes_:P_VkPresentModeKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkDestroySurfaceKHR(
    instance_:VkInstance;
    surface_:VkSurfaceKHR;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceSupportKHR(
    physicalDevice_:VkPhysicalDevice;
    queueFamilyIndex_:T_uint32_t;
    surface_:VkSurfaceKHR;
    pSupported_:P_VkBool32 ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceCapabilitiesKHR(
    physicalDevice_:VkPhysicalDevice;
    surface_:VkSurfaceKHR;
    pSurfaceCapabilities_:P_VkSurfaceCapabilitiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceFormatsKHR(
    physicalDevice_:VkPhysicalDevice;
    surface_:VkSurfaceKHR;
    pSurfaceFormatCount_:P_uint32_t;
    pSurfaceFormats_:P_VkSurfaceFormatKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfacePresentModesKHR(
    physicalDevice_:VkPhysicalDevice;
    surface_:VkSurfaceKHR;
    pPresentModeCount_:P_uint32_t;
    pPresentModes_:P_VkPresentModeKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_swapchain = 1;
type P_VkSwapchainKHR = ^VkSwapchainKHR;
     VkSwapchainKHR = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_KHR_SWAPCHAIN_SPEC_VERSION     = 70;
const VK_KHR_SWAPCHAIN_EXTENSION_NAME = 'VK_KHR_swapchain';

type P_VkSwapchainCreateFlagBitsKHR = ^VkSwapchainCreateFlagBitsKHR;
     VkSwapchainCreateFlagBitsKHR = (
       VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = $00000001,
       VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR = $00000002,
       VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR = $00000004,
       VK_SWAPCHAIN_CREATE_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkSwapchainCreateFlagsKHR = ^VkSwapchainCreateFlagsKHR;
     VkSwapchainCreateFlagsKHR = VkFlags;

type P_VkDeviceGroupPresentModeFlagBitsKHR = ^VkDeviceGroupPresentModeFlagBitsKHR;
     VkDeviceGroupPresentModeFlagBitsKHR = (
       VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = $00000001,
       VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = $00000002,
       VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = $00000004,
       VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = $00000008,
       VK_DEVICE_GROUP_PRESENT_MODE_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkDeviceGroupPresentModeFlagsKHR = ^VkDeviceGroupPresentModeFlagsKHR;
     VkDeviceGroupPresentModeFlagsKHR = VkFlags;
type P_VkSwapchainCreateInfoKHR = ^VkSwapchainCreateInfoKHR;
     VkSwapchainCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkSwapchainCreateFlagsKHR;
       surface :VkSurfaceKHR;
       minImageCount :T_uint32_t;
       imageFormat :VkFormat;
       imageColorSpace :VkColorSpaceKHR;
       imageExtent :VkExtent2D;
       imageArrayLayers :T_uint32_t;
       imageUsage :VkImageUsageFlags;
       imageSharingMode :VkSharingMode;
       queueFamilyIndexCount :T_uint32_t;
       pQueueFamilyIndices :P_uint32_t;
       preTransform :VkSurfaceTransformFlagBitsKHR;
       compositeAlpha :VkCompositeAlphaFlagBitsKHR;
       presentMode :VkPresentModeKHR;
       clipped :VkBool32;
       oldSwapchain :VkSwapchainKHR;
     end;

type P_VkPresentInfoKHR = ^VkPresentInfoKHR;
     VkPresentInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       waitSemaphoreCount :T_uint32_t;
       pWaitSemaphores :P_VkSemaphore;
       swapchainCount :T_uint32_t;
       pSwapchains :P_VkSwapchainKHR;
       pImageIndices :P_uint32_t;
       pResults :P_VkResult;
     end;

type P_VkImageSwapchainCreateInfoKHR = ^VkImageSwapchainCreateInfoKHR;
     VkImageSwapchainCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       swapchain :VkSwapchainKHR;
     end;

type P_VkBindImageMemorySwapchainInfoKHR = ^VkBindImageMemorySwapchainInfoKHR;
     VkBindImageMemorySwapchainInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       swapchain :VkSwapchainKHR;
       imageIndex :T_uint32_t;
     end;

type P_VkAcquireNextImageInfoKHR = ^VkAcquireNextImageInfoKHR;
     VkAcquireNextImageInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       swapchain :VkSwapchainKHR;
       timeout :T_uint64_t;
       semaphore :VkSemaphore;
       fence :VkFence;
       deviceMask :T_uint32_t;
     end;

type P_VkDeviceGroupPresentCapabilitiesKHR = ^VkDeviceGroupPresentCapabilitiesKHR;
     VkDeviceGroupPresentCapabilitiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       presentMask :array [ 0..VK_MAX_DEVICE_GROUP_SIZE-1 ] of T_uint32_t;
       modes :VkDeviceGroupPresentModeFlagsKHR;
     end;

type P_VkDeviceGroupPresentInfoKHR = ^VkDeviceGroupPresentInfoKHR;
     VkDeviceGroupPresentInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       swapchainCount :T_uint32_t;
       pDeviceMasks :P_uint32_t;
       mode :VkDeviceGroupPresentModeFlagBitsKHR;
     end;

type P_VkDeviceGroupSwapchainCreateInfoKHR = ^VkDeviceGroupSwapchainCreateInfoKHR;
     VkDeviceGroupSwapchainCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       modes :VkDeviceGroupPresentModeFlagsKHR;
     end;

type PFN_vkCreateSwapchainKHR = function( device_:VkDevice; const pCreateInfo_:P_VkSwapchainCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSwapchain_:P_VkSwapchainKHR ) :VkResult;
type PFN_vkDestroySwapchainKHR = procedure( device_:VkDevice; swapchain_:VkSwapchainKHR; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetSwapchainImagesKHR = function( device_:VkDevice; swapchain_:VkSwapchainKHR; pSwapchainImageCount_:P_uint32_t; pSwapchainImages_:P_VkImage ) :VkResult;
type PFN_vkAcquireNextImageKHR = function( device_:VkDevice; swapchain_:VkSwapchainKHR; timeout_:T_uint64_t; semaphore_:VkSemaphore; fence_:VkFence; pImageIndex_:P_uint32_t ) :VkResult;
type PFN_vkQueuePresentKHR = function( queue_:VkQueue; const pPresentInfo_:P_VkPresentInfoKHR ) :VkResult;
type PFN_vkGetDeviceGroupPresentCapabilitiesKHR = function( device_:VkDevice; pDeviceGroupPresentCapabilities_:P_VkDeviceGroupPresentCapabilitiesKHR ) :VkResult;
type PFN_vkGetDeviceGroupSurfacePresentModesKHR = function( device_:VkDevice; surface_:VkSurfaceKHR; pModes_:P_VkDeviceGroupPresentModeFlagsKHR ) :VkResult;
type PFN_vkGetPhysicalDevicePresentRectanglesKHR = function( physicalDevice_:VkPhysicalDevice; surface_:VkSurfaceKHR; pRectCount_:P_uint32_t; pRects_:P_VkRect2D ) :VkResult;
type PFN_vkAcquireNextImage2KHR = function( device_:VkDevice; const pAcquireInfo_:P_VkAcquireNextImageInfoKHR; pImageIndex_:P_uint32_t ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateSwapchainKHR(
    device_:VkDevice;
    pCreateInfo_:P_VkSwapchainCreateInfoKHR;
    pAllocator_:P_VkAllocationCallbacks;
    pSwapchain_:P_VkSwapchainKHR ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroySwapchainKHR(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetSwapchainImagesKHR(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR;
    pSwapchainImageCount_:P_uint32_t;
    pSwapchainImages_:P_VkImage ) :VkResult; stdcall; external DLLNAME;

function vkAcquireNextImageKHR(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR;
    timeout_:T_uint64_t;
    semaphore_:VkSemaphore;
    fence_:VkFence;
    pImageIndex_:P_uint32_t ) :VkResult; stdcall; external DLLNAME;

function vkQueuePresentKHR(
    queue_:VkQueue;
    pPresentInfo_:P_VkPresentInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDeviceGroupPresentCapabilitiesKHR(
    device_:VkDevice;
    pDeviceGroupPresentCapabilities_:P_VkDeviceGroupPresentCapabilitiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDeviceGroupSurfacePresentModesKHR(
    device_:VkDevice;
    surface_:VkSurfaceKHR;
    pModes_:P_VkDeviceGroupPresentModeFlagsKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDevicePresentRectanglesKHR(
    physicalDevice_:VkPhysicalDevice;
    surface_:VkSurfaceKHR;
    pRectCount_:P_uint32_t;
    pRects_:P_VkRect2D ) :VkResult; stdcall; external DLLNAME;

function vkAcquireNextImage2KHR(
    device_:VkDevice;
    pAcquireInfo_:P_VkAcquireNextImageInfoKHR;
    pImageIndex_:P_uint32_t ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_display = 1;
type P_VkDisplayKHR = ^VkDisplayKHR;
     VkDisplayKHR = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
type P_VkDisplayModeKHR = ^VkDisplayModeKHR;
     VkDisplayModeKHR = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_KHR_DISPLAY_SPEC_VERSION       = 23;
const VK_KHR_DISPLAY_EXTENSION_NAME = 'VK_KHR_display';
type P_VkDisplayModeCreateFlagsKHR = ^VkDisplayModeCreateFlagsKHR;
     VkDisplayModeCreateFlagsKHR = VkFlags;

type P_VkDisplayPlaneAlphaFlagBitsKHR = ^VkDisplayPlaneAlphaFlagBitsKHR;
     VkDisplayPlaneAlphaFlagBitsKHR = (
       VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = $00000001,
       VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = $00000002,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = $00000004,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = $00000008,
       VK_DISPLAY_PLANE_ALPHA_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkDisplayPlaneAlphaFlagsKHR = ^VkDisplayPlaneAlphaFlagsKHR;
     VkDisplayPlaneAlphaFlagsKHR = VkFlags;
type P_VkDisplaySurfaceCreateFlagsKHR = ^VkDisplaySurfaceCreateFlagsKHR;
     VkDisplaySurfaceCreateFlagsKHR = VkFlags;
type P_VkDisplayModeParametersKHR = ^VkDisplayModeParametersKHR;
     VkDisplayModeParametersKHR = record
       visibleRegion :VkExtent2D;
       refreshRate :T_uint32_t;
     end;

type P_VkDisplayModeCreateInfoKHR = ^VkDisplayModeCreateInfoKHR;
     VkDisplayModeCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDisplayModeCreateFlagsKHR;
       parameters :VkDisplayModeParametersKHR;
     end;

type P_VkDisplayModePropertiesKHR = ^VkDisplayModePropertiesKHR;
     VkDisplayModePropertiesKHR = record
       displayMode :VkDisplayModeKHR;
       parameters :VkDisplayModeParametersKHR;
     end;

type P_VkDisplayPlaneCapabilitiesKHR = ^VkDisplayPlaneCapabilitiesKHR;
     VkDisplayPlaneCapabilitiesKHR = record
       supportedAlpha :VkDisplayPlaneAlphaFlagsKHR;
       minSrcPosition :VkOffset2D;
       maxSrcPosition :VkOffset2D;
       minSrcExtent :VkExtent2D;
       maxSrcExtent :VkExtent2D;
       minDstPosition :VkOffset2D;
       maxDstPosition :VkOffset2D;
       minDstExtent :VkExtent2D;
       maxDstExtent :VkExtent2D;
     end;

type P_VkDisplayPlanePropertiesKHR = ^VkDisplayPlanePropertiesKHR;
     VkDisplayPlanePropertiesKHR = record
       currentDisplay :VkDisplayKHR;
       currentStackIndex :T_uint32_t;
     end;

type P_VkDisplayPropertiesKHR = ^VkDisplayPropertiesKHR;
     VkDisplayPropertiesKHR = record
       display :VkDisplayKHR;
       displayName :P_char;
       physicalDimensions :VkExtent2D;
       physicalResolution :VkExtent2D;
       supportedTransforms :VkSurfaceTransformFlagsKHR;
       planeReorderPossible :VkBool32;
       persistentContent :VkBool32;
     end;

type P_VkDisplaySurfaceCreateInfoKHR = ^VkDisplaySurfaceCreateInfoKHR;
     VkDisplaySurfaceCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDisplaySurfaceCreateFlagsKHR;
       displayMode :VkDisplayModeKHR;
       planeIndex :T_uint32_t;
       planeStackIndex :T_uint32_t;
       transform :VkSurfaceTransformFlagBitsKHR;
       globalAlpha :T_float;
       alphaMode :VkDisplayPlaneAlphaFlagBitsKHR;
       imageExtent :VkExtent2D;
     end;

type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR = function( physicalDevice_:VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayPropertiesKHR ) :VkResult;
type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = function( physicalDevice_:VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayPlanePropertiesKHR ) :VkResult;
type PFN_vkGetDisplayPlaneSupportedDisplaysKHR = function( physicalDevice_:VkPhysicalDevice; planeIndex_:T_uint32_t; pDisplayCount_:P_uint32_t; pDisplays_:P_VkDisplayKHR ) :VkResult;
type PFN_vkGetDisplayModePropertiesKHR = function( physicalDevice_:VkPhysicalDevice; display_:VkDisplayKHR; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayModePropertiesKHR ) :VkResult;
type PFN_vkCreateDisplayModeKHR = function( physicalDevice_:VkPhysicalDevice; display_:VkDisplayKHR; const pCreateInfo_:P_VkDisplayModeCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pMode_:P_VkDisplayModeKHR ) :VkResult;
type PFN_vkGetDisplayPlaneCapabilitiesKHR = function( physicalDevice_:VkPhysicalDevice; mode_:VkDisplayModeKHR; planeIndex_:T_uint32_t; pCapabilities_:P_VkDisplayPlaneCapabilitiesKHR ) :VkResult;
type PFN_vkCreateDisplayPlaneSurfaceKHR = function( instance_:VkInstance; const pCreateInfo_:P_VkDisplaySurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceDisplayPropertiesKHR(
    physicalDevice_:VkPhysicalDevice;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkDisplayPropertiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceDisplayPlanePropertiesKHR(
    physicalDevice_:VkPhysicalDevice;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkDisplayPlanePropertiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDisplayPlaneSupportedDisplaysKHR(
    physicalDevice_:VkPhysicalDevice;
    planeIndex_:T_uint32_t;
    pDisplayCount_:P_uint32_t;
    pDisplays_:P_VkDisplayKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDisplayModePropertiesKHR(
    physicalDevice_:VkPhysicalDevice;
    display_:VkDisplayKHR;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkDisplayModePropertiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkCreateDisplayModeKHR(
    physicalDevice_:VkPhysicalDevice;
    display_:VkDisplayKHR;
    pCreateInfo_:P_VkDisplayModeCreateInfoKHR;
    pAllocator_:P_VkAllocationCallbacks;
    pMode_:P_VkDisplayModeKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDisplayPlaneCapabilitiesKHR(
    physicalDevice_:VkPhysicalDevice;
    mode_:VkDisplayModeKHR;
    planeIndex_:T_uint32_t;
    pCapabilities_:P_VkDisplayPlaneCapabilitiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkCreateDisplayPlaneSurfaceKHR(
    instance_:VkInstance;
    pCreateInfo_:P_VkDisplaySurfaceCreateInfoKHR;
    pAllocator_:P_VkAllocationCallbacks;
    pSurface_:P_VkSurfaceKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_display_swapchain = 1;
const VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 10;
const VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = 'VK_KHR_display_swapchain';
type P_VkDisplayPresentInfoKHR = ^VkDisplayPresentInfoKHR;
     VkDisplayPresentInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcRect :VkRect2D;
       dstRect :VkRect2D;
       persistent :VkBool32;
     end;

type PFN_vkCreateSharedSwapchainsKHR = function( device_:VkDevice; swapchainCount_:T_uint32_t; const pCreateInfos_:P_VkSwapchainCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSwapchains_:P_VkSwapchainKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateSharedSwapchainsKHR(
    device_:VkDevice;
    swapchainCount_:T_uint32_t;
    pCreateInfos_:P_VkSwapchainCreateInfoKHR;
    pAllocator_:P_VkAllocationCallbacks;
    pSwapchains_:P_VkSwapchainKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_sampler_mirror_clamp_to_edge = 1;
const VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3;
const VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = 'VK_KHR_sampler_mirror_clamp_to_edge';


const VK_KHR_multiview = 1;
const VK_KHR_MULTIVIEW_SPEC_VERSION     = 1;
const VK_KHR_MULTIVIEW_EXTENSION_NAME = 'VK_KHR_multiview';
type P_VkRenderPassMultiviewCreateInfoKHR = ^VkRenderPassMultiviewCreateInfoKHR;
     VkRenderPassMultiviewCreateInfoKHR = VkRenderPassMultiviewCreateInfo;

type P_VkPhysicalDeviceMultiviewFeaturesKHR = ^VkPhysicalDeviceMultiviewFeaturesKHR;
     VkPhysicalDeviceMultiviewFeaturesKHR = VkPhysicalDeviceMultiviewFeatures;

type P_VkPhysicalDeviceMultiviewPropertiesKHR = ^VkPhysicalDeviceMultiviewPropertiesKHR;
     VkPhysicalDeviceMultiviewPropertiesKHR = VkPhysicalDeviceMultiviewProperties;



const VK_KHR_get_physical_device_properties2 = 1;
const VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 2;
const VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = 'VK_KHR_get_physical_device_properties2';
type P_VkPhysicalDeviceFeatures2KHR = ^VkPhysicalDeviceFeatures2KHR;
     VkPhysicalDeviceFeatures2KHR = VkPhysicalDeviceFeatures2;

type P_VkPhysicalDeviceProperties2KHR = ^VkPhysicalDeviceProperties2KHR;
     VkPhysicalDeviceProperties2KHR = VkPhysicalDeviceProperties2;

type P_VkFormatProperties2KHR = ^VkFormatProperties2KHR;
     VkFormatProperties2KHR = VkFormatProperties2;

type P_VkImageFormatProperties2KHR = ^VkImageFormatProperties2KHR;
     VkImageFormatProperties2KHR = VkImageFormatProperties2;

type P_VkPhysicalDeviceImageFormatInfo2KHR = ^VkPhysicalDeviceImageFormatInfo2KHR;
     VkPhysicalDeviceImageFormatInfo2KHR = VkPhysicalDeviceImageFormatInfo2;

type P_VkQueueFamilyProperties2KHR = ^VkQueueFamilyProperties2KHR;
     VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2;

type P_VkPhysicalDeviceMemoryProperties2KHR = ^VkPhysicalDeviceMemoryProperties2KHR;
     VkPhysicalDeviceMemoryProperties2KHR = VkPhysicalDeviceMemoryProperties2;

type P_VkSparseImageFormatProperties2KHR = ^VkSparseImageFormatProperties2KHR;
     VkSparseImageFormatProperties2KHR = VkSparseImageFormatProperties2;

type P_VkPhysicalDeviceSparseImageFormatInfo2KHR = ^VkPhysicalDeviceSparseImageFormatInfo2KHR;
     VkPhysicalDeviceSparseImageFormatInfo2KHR = VkPhysicalDeviceSparseImageFormatInfo2;

type PFN_vkGetPhysicalDeviceFeatures2KHR = procedure( physicalDevice_:VkPhysicalDevice; pFeatures_:P_VkPhysicalDeviceFeatures2 );
type PFN_vkGetPhysicalDeviceProperties2KHR = procedure( physicalDevice_:VkPhysicalDevice; pProperties_:P_VkPhysicalDeviceProperties2 );
type PFN_vkGetPhysicalDeviceFormatProperties2KHR = procedure( physicalDevice_:VkPhysicalDevice; format_:VkFormat; pFormatProperties_:P_VkFormatProperties2 );
type PFN_vkGetPhysicalDeviceImageFormatProperties2KHR = function( physicalDevice_:VkPhysicalDevice; const pImageFormatInfo_:P_VkPhysicalDeviceImageFormatInfo2; pImageFormatProperties_:P_VkImageFormatProperties2 ) :VkResult;
type PFN_vkGetPhysicalDeviceQueueFamilyProperties2KHR = procedure( physicalDevice_:VkPhysicalDevice; pQueueFamilyPropertyCount_:P_uint32_t; pQueueFamilyProperties_:P_VkQueueFamilyProperties2 );
type PFN_vkGetPhysicalDeviceMemoryProperties2KHR = procedure( physicalDevice_:VkPhysicalDevice; pMemoryProperties_:P_VkPhysicalDeviceMemoryProperties2 );
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2KHR = procedure( physicalDevice_:VkPhysicalDevice; const pFormatInfo_:P_VkPhysicalDeviceSparseImageFormatInfo2; pPropertyCount_:P_uint32_t; pProperties_:P_VkSparseImageFormatProperties2 );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetPhysicalDeviceFeatures2KHR(
    physicalDevice_:VkPhysicalDevice;
    pFeatures_:P_VkPhysicalDeviceFeatures2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pProperties_:P_VkPhysicalDeviceProperties2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFormatProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    format_:VkFormat;
    pFormatProperties_:P_VkFormatProperties2 ); stdcall; external DLLNAME;

function vkGetPhysicalDeviceImageFormatProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pImageFormatInfo_:P_VkPhysicalDeviceImageFormatInfo2;
    pImageFormatProperties_:P_VkImageFormatProperties2 ) :VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceQueueFamilyProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pQueueFamilyPropertyCount_:P_uint32_t;
    pQueueFamilyProperties_:P_VkQueueFamilyProperties2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceMemoryProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pMemoryProperties_:P_VkPhysicalDeviceMemoryProperties2 ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceSparseImageFormatProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pFormatInfo_:P_VkPhysicalDeviceSparseImageFormatInfo2;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkSparseImageFormatProperties2 ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_device_group = 1;
const VK_KHR_DEVICE_GROUP_SPEC_VERSION  = 4;
const VK_KHR_DEVICE_GROUP_EXTENSION_NAME = 'VK_KHR_device_group';
type P_VkPeerMemoryFeatureFlagsKHR = ^VkPeerMemoryFeatureFlagsKHR;
     VkPeerMemoryFeatureFlagsKHR = VkPeerMemoryFeatureFlags;

type P_VkPeerMemoryFeatureFlagBitsKHR = ^VkPeerMemoryFeatureFlagBitsKHR;
     VkPeerMemoryFeatureFlagBitsKHR = VkPeerMemoryFeatureFlagBits;

type P_VkMemoryAllocateFlagsKHR = ^VkMemoryAllocateFlagsKHR;
     VkMemoryAllocateFlagsKHR = VkMemoryAllocateFlags;

type P_VkMemoryAllocateFlagBitsKHR = ^VkMemoryAllocateFlagBitsKHR;
     VkMemoryAllocateFlagBitsKHR = VkMemoryAllocateFlagBits;

type P_VkMemoryAllocateFlagsInfoKHR = ^VkMemoryAllocateFlagsInfoKHR;
     VkMemoryAllocateFlagsInfoKHR = VkMemoryAllocateFlagsInfo;

type P_VkDeviceGroupRenderPassBeginInfoKHR = ^VkDeviceGroupRenderPassBeginInfoKHR;
     VkDeviceGroupRenderPassBeginInfoKHR = VkDeviceGroupRenderPassBeginInfo;

type P_VkDeviceGroupCommandBufferBeginInfoKHR = ^VkDeviceGroupCommandBufferBeginInfoKHR;
     VkDeviceGroupCommandBufferBeginInfoKHR = VkDeviceGroupCommandBufferBeginInfo;

type P_VkDeviceGroupSubmitInfoKHR = ^VkDeviceGroupSubmitInfoKHR;
     VkDeviceGroupSubmitInfoKHR = VkDeviceGroupSubmitInfo;

type P_VkDeviceGroupBindSparseInfoKHR = ^VkDeviceGroupBindSparseInfoKHR;
     VkDeviceGroupBindSparseInfoKHR = VkDeviceGroupBindSparseInfo;

type P_VkBindBufferMemoryDeviceGroupInfoKHR = ^VkBindBufferMemoryDeviceGroupInfoKHR;
     VkBindBufferMemoryDeviceGroupInfoKHR = VkBindBufferMemoryDeviceGroupInfo;

type P_VkBindImageMemoryDeviceGroupInfoKHR = ^VkBindImageMemoryDeviceGroupInfoKHR;
     VkBindImageMemoryDeviceGroupInfoKHR = VkBindImageMemoryDeviceGroupInfo;

type PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR = procedure( device_:VkDevice; heapIndex_:T_uint32_t; localDeviceIndex_:T_uint32_t; remoteDeviceIndex_:T_uint32_t; pPeerMemoryFeatures_:P_VkPeerMemoryFeatureFlags );
type PFN_vkCmdSetDeviceMaskKHR = procedure( commandBuffer_:VkCommandBuffer; deviceMask_:T_uint32_t );
type PFN_vkCmdDispatchBaseKHR = procedure( commandBuffer_:VkCommandBuffer; baseGroupX_:T_uint32_t; baseGroupY_:T_uint32_t; baseGroupZ_:T_uint32_t; groupCountX_:T_uint32_t; groupCountY_:T_uint32_t; groupCountZ_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetDeviceGroupPeerMemoryFeaturesKHR(
    device_:VkDevice;
    heapIndex_:T_uint32_t;
    localDeviceIndex_:T_uint32_t;
    remoteDeviceIndex_:T_uint32_t;
    pPeerMemoryFeatures_:P_VkPeerMemoryFeatureFlags ); stdcall; external DLLNAME;

procedure vkCmdSetDeviceMaskKHR(
    commandBuffer_:VkCommandBuffer;
    deviceMask_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDispatchBaseKHR(
    commandBuffer_:VkCommandBuffer;
    baseGroupX_:T_uint32_t;
    baseGroupY_:T_uint32_t;
    baseGroupZ_:T_uint32_t;
    groupCountX_:T_uint32_t;
    groupCountY_:T_uint32_t;
    groupCountZ_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_shader_draw_parameters = 1;
const VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1;
const VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = 'VK_KHR_shader_draw_parameters';


const VK_KHR_maintenance1 = 1;
const VK_KHR_MAINTENANCE1_SPEC_VERSION  = 2;
const VK_KHR_MAINTENANCE1_EXTENSION_NAME = 'VK_KHR_maintenance1';
type P_VkCommandPoolTrimFlagsKHR = ^VkCommandPoolTrimFlagsKHR;
     VkCommandPoolTrimFlagsKHR = VkCommandPoolTrimFlags;

type PFN_vkTrimCommandPoolKHR = procedure( device_:VkDevice; commandPool_:VkCommandPool; flags_:VkCommandPoolTrimFlags );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkTrimCommandPoolKHR(
    device_:VkDevice;
    commandPool_:VkCommandPool;
    flags_:VkCommandPoolTrimFlags ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_device_group_creation = 1;
const VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1;
const VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME = 'VK_KHR_device_group_creation';
const VK_MAX_DEVICE_GROUP_SIZE_KHR      = VK_MAX_DEVICE_GROUP_SIZE;
type P_VkPhysicalDeviceGroupPropertiesKHR = ^VkPhysicalDeviceGroupPropertiesKHR;
     VkPhysicalDeviceGroupPropertiesKHR = VkPhysicalDeviceGroupProperties;

type P_VkDeviceGroupDeviceCreateInfoKHR = ^VkDeviceGroupDeviceCreateInfoKHR;
     VkDeviceGroupDeviceCreateInfoKHR = VkDeviceGroupDeviceCreateInfo;

type PFN_vkEnumeratePhysicalDeviceGroupsKHR = function( instance_:VkInstance; pPhysicalDeviceGroupCount_:P_uint32_t; pPhysicalDeviceGroupProperties_:P_VkPhysicalDeviceGroupProperties ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkEnumeratePhysicalDeviceGroupsKHR(
    instance_:VkInstance;
    pPhysicalDeviceGroupCount_:P_uint32_t;
    pPhysicalDeviceGroupProperties_:P_VkPhysicalDeviceGroupProperties ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_memory_capabilities = 1;
const VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = 'VK_KHR_external_memory_capabilities';
const VK_LUID_SIZE_KHR                  = VK_LUID_SIZE;
type P_VkExternalMemoryHandleTypeFlagsKHR = ^VkExternalMemoryHandleTypeFlagsKHR;
     VkExternalMemoryHandleTypeFlagsKHR = VkExternalMemoryHandleTypeFlags;

type P_VkExternalMemoryHandleTypeFlagBitsKHR = ^VkExternalMemoryHandleTypeFlagBitsKHR;
     VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBits;

type P_VkExternalMemoryFeatureFlagsKHR = ^VkExternalMemoryFeatureFlagsKHR;
     VkExternalMemoryFeatureFlagsKHR = VkExternalMemoryFeatureFlags;

type P_VkExternalMemoryFeatureFlagBitsKHR = ^VkExternalMemoryFeatureFlagBitsKHR;
     VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBits;

type P_VkExternalMemoryPropertiesKHR = ^VkExternalMemoryPropertiesKHR;
     VkExternalMemoryPropertiesKHR = VkExternalMemoryProperties;

type P_VkPhysicalDeviceExternalImageFormatInfoKHR = ^VkPhysicalDeviceExternalImageFormatInfoKHR;
     VkPhysicalDeviceExternalImageFormatInfoKHR = VkPhysicalDeviceExternalImageFormatInfo;

type P_VkExternalImageFormatPropertiesKHR = ^VkExternalImageFormatPropertiesKHR;
     VkExternalImageFormatPropertiesKHR = VkExternalImageFormatProperties;

type P_VkPhysicalDeviceExternalBufferInfoKHR = ^VkPhysicalDeviceExternalBufferInfoKHR;
     VkPhysicalDeviceExternalBufferInfoKHR = VkPhysicalDeviceExternalBufferInfo;

type P_VkExternalBufferPropertiesKHR = ^VkExternalBufferPropertiesKHR;
     VkExternalBufferPropertiesKHR = VkExternalBufferProperties;

type P_VkPhysicalDeviceIDPropertiesKHR = ^VkPhysicalDeviceIDPropertiesKHR;
     VkPhysicalDeviceIDPropertiesKHR = VkPhysicalDeviceIDProperties;

type PFN_vkGetPhysicalDeviceExternalBufferPropertiesKHR = procedure( physicalDevice_:VkPhysicalDevice; const pExternalBufferInfo_:P_VkPhysicalDeviceExternalBufferInfo; pExternalBufferProperties_:P_VkExternalBufferProperties );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetPhysicalDeviceExternalBufferPropertiesKHR(
    physicalDevice_:VkPhysicalDevice;
    pExternalBufferInfo_:P_VkPhysicalDeviceExternalBufferInfo;
    pExternalBufferProperties_:P_VkExternalBufferProperties ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_memory = 1;
const VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME = 'VK_KHR_external_memory';
const VK_QUEUE_FAMILY_EXTERNAL_KHR      = VK_QUEUE_FAMILY_EXTERNAL;
type P_VkExternalMemoryImageCreateInfoKHR = ^VkExternalMemoryImageCreateInfoKHR;
     VkExternalMemoryImageCreateInfoKHR = VkExternalMemoryImageCreateInfo;

type P_VkExternalMemoryBufferCreateInfoKHR = ^VkExternalMemoryBufferCreateInfoKHR;
     VkExternalMemoryBufferCreateInfoKHR = VkExternalMemoryBufferCreateInfo;

type P_VkExportMemoryAllocateInfoKHR = ^VkExportMemoryAllocateInfoKHR;
     VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfo;



const VK_KHR_external_memory_fd = 1;
const VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = 'VK_KHR_external_memory_fd';
type P_VkImportMemoryFdInfoKHR = ^VkImportMemoryFdInfoKHR;
     VkImportMemoryFdInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       handleType :VkExternalMemoryHandleTypeFlagBits;
       fd :T_int;
     end;

type P_VkMemoryFdPropertiesKHR = ^VkMemoryFdPropertiesKHR;
     VkMemoryFdPropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       memoryTypeBits :T_uint32_t;
     end;

type P_VkMemoryGetFdInfoKHR = ^VkMemoryGetFdInfoKHR;
     VkMemoryGetFdInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       memory :VkDeviceMemory;
       handleType :VkExternalMemoryHandleTypeFlagBits;
     end;

type PFN_vkGetMemoryFdKHR = function( device_:VkDevice; const pGetFdInfo_:P_VkMemoryGetFdInfoKHR; pFd_:P_int ) :VkResult;
type PFN_vkGetMemoryFdPropertiesKHR = function( device_:VkDevice; handleType_:VkExternalMemoryHandleTypeFlagBits; fd_:T_int; pMemoryFdProperties_:P_VkMemoryFdPropertiesKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetMemoryFdKHR(
    device_:VkDevice;
    pGetFdInfo_:P_VkMemoryGetFdInfoKHR;
    pFd_:P_int ) :VkResult; stdcall; external DLLNAME;

function vkGetMemoryFdPropertiesKHR(
    device_:VkDevice;
    handleType_:VkExternalMemoryHandleTypeFlagBits;
    fd_:T_int;
    pMemoryFdProperties_:P_VkMemoryFdPropertiesKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_semaphore_capabilities = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME = 'VK_KHR_external_semaphore_capabilities';
type P_VkExternalSemaphoreHandleTypeFlagsKHR = ^VkExternalSemaphoreHandleTypeFlagsKHR;
     VkExternalSemaphoreHandleTypeFlagsKHR = VkExternalSemaphoreHandleTypeFlags;

type P_VkExternalSemaphoreHandleTypeFlagBitsKHR = ^VkExternalSemaphoreHandleTypeFlagBitsKHR;
     VkExternalSemaphoreHandleTypeFlagBitsKHR = VkExternalSemaphoreHandleTypeFlagBits;

type P_VkExternalSemaphoreFeatureFlagsKHR = ^VkExternalSemaphoreFeatureFlagsKHR;
     VkExternalSemaphoreFeatureFlagsKHR = VkExternalSemaphoreFeatureFlags;

type P_VkExternalSemaphoreFeatureFlagBitsKHR = ^VkExternalSemaphoreFeatureFlagBitsKHR;
     VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBits;

type P_VkPhysicalDeviceExternalSemaphoreInfoKHR = ^VkPhysicalDeviceExternalSemaphoreInfoKHR;
     VkPhysicalDeviceExternalSemaphoreInfoKHR = VkPhysicalDeviceExternalSemaphoreInfo;

type P_VkExternalSemaphorePropertiesKHR = ^VkExternalSemaphorePropertiesKHR;
     VkExternalSemaphorePropertiesKHR = VkExternalSemaphoreProperties;

type PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR = procedure( physicalDevice_:VkPhysicalDevice; const pExternalSemaphoreInfo_:P_VkPhysicalDeviceExternalSemaphoreInfo; pExternalSemaphoreProperties_:P_VkExternalSemaphoreProperties );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetPhysicalDeviceExternalSemaphorePropertiesKHR(
    physicalDevice_:VkPhysicalDevice;
    pExternalSemaphoreInfo_:P_VkPhysicalDeviceExternalSemaphoreInfo;
    pExternalSemaphoreProperties_:P_VkExternalSemaphoreProperties ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_semaphore = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = 'VK_KHR_external_semaphore';
type P_VkSemaphoreImportFlagsKHR = ^VkSemaphoreImportFlagsKHR;
     VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlags;

type P_VkSemaphoreImportFlagBitsKHR = ^VkSemaphoreImportFlagBitsKHR;
     VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBits;

type P_VkExportSemaphoreCreateInfoKHR = ^VkExportSemaphoreCreateInfoKHR;
     VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfo;



const VK_KHR_external_semaphore_fd = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = 'VK_KHR_external_semaphore_fd';
type P_VkImportSemaphoreFdInfoKHR = ^VkImportSemaphoreFdInfoKHR;
     VkImportSemaphoreFdInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       semaphore :VkSemaphore;
       flags :VkSemaphoreImportFlags;
       handleType :VkExternalSemaphoreHandleTypeFlagBits;
       fd :T_int;
     end;

type P_VkSemaphoreGetFdInfoKHR = ^VkSemaphoreGetFdInfoKHR;
     VkSemaphoreGetFdInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       semaphore :VkSemaphore;
       handleType :VkExternalSemaphoreHandleTypeFlagBits;
     end;

type PFN_vkImportSemaphoreFdKHR = function( device_:VkDevice; const pImportSemaphoreFdInfo_:P_VkImportSemaphoreFdInfoKHR ) :VkResult;
type PFN_vkGetSemaphoreFdKHR = function( device_:VkDevice; const pGetFdInfo_:P_VkSemaphoreGetFdInfoKHR; pFd_:P_int ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkImportSemaphoreFdKHR(
    device_:VkDevice;
    pImportSemaphoreFdInfo_:P_VkImportSemaphoreFdInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetSemaphoreFdKHR(
    device_:VkDevice;
    pGetFdInfo_:P_VkSemaphoreGetFdInfoKHR;
    pFd_:P_int ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_push_descriptor = 1;
const VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2;
const VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = 'VK_KHR_push_descriptor';
type P_VkPhysicalDevicePushDescriptorPropertiesKHR = ^VkPhysicalDevicePushDescriptorPropertiesKHR;
     VkPhysicalDevicePushDescriptorPropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       maxPushDescriptors :T_uint32_t;
     end;

type PFN_vkCmdPushDescriptorSetKHR = procedure( commandBuffer_:VkCommandBuffer; pipelineBindPoint_:VkPipelineBindPoint; layout_:VkPipelineLayout; set_:T_uint32_t; descriptorWriteCount_:T_uint32_t; const pDescriptorWrites_:P_VkWriteDescriptorSet );
type PFN_vkCmdPushDescriptorSetWithTemplateKHR = procedure( commandBuffer_:VkCommandBuffer; descriptorUpdateTemplate_:VkDescriptorUpdateTemplate; layout_:VkPipelineLayout; set_:T_uint32_t; const pData_:P_void );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdPushDescriptorSetKHR(
    commandBuffer_:VkCommandBuffer;
    pipelineBindPoint_:VkPipelineBindPoint;
    layout_:VkPipelineLayout;
    set_:T_uint32_t;
    descriptorWriteCount_:T_uint32_t;
    pDescriptorWrites_:P_VkWriteDescriptorSet ); stdcall; external DLLNAME;

procedure vkCmdPushDescriptorSetWithTemplateKHR(
    commandBuffer_:VkCommandBuffer;
    descriptorUpdateTemplate_:VkDescriptorUpdateTemplate;
    layout_:VkPipelineLayout;
    set_:T_uint32_t;
    pData_:P_void ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_shader_float16_int8 = 1;
const VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION = 1;
const VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME = 'VK_KHR_shader_float16_int8';
type P_VkPhysicalDeviceShaderFloat16Int8FeaturesKHR = ^VkPhysicalDeviceShaderFloat16Int8FeaturesKHR;
     VkPhysicalDeviceShaderFloat16Int8FeaturesKHR = VkPhysicalDeviceShaderFloat16Int8Features;

type P_VkPhysicalDeviceFloat16Int8FeaturesKHR = ^VkPhysicalDeviceFloat16Int8FeaturesKHR;
     VkPhysicalDeviceFloat16Int8FeaturesKHR = VkPhysicalDeviceShaderFloat16Int8Features;



const VK_KHR_16bit_storage = 1;
const VK_KHR_16BIT_STORAGE_SPEC_VERSION = 1;
const VK_KHR_16BIT_STORAGE_EXTENSION_NAME = 'VK_KHR_16bit_storage';
type P_VkPhysicalDevice16BitStorageFeaturesKHR = ^VkPhysicalDevice16BitStorageFeaturesKHR;
     VkPhysicalDevice16BitStorageFeaturesKHR = VkPhysicalDevice16BitStorageFeatures;



const VK_KHR_incremental_present = 1;
const VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1;
const VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME = 'VK_KHR_incremental_present';
type P_VkRectLayerKHR = ^VkRectLayerKHR;
     VkRectLayerKHR = record
       offset :VkOffset2D;
       extent :VkExtent2D;
       layer :T_uint32_t;
     end;

type P_VkPresentRegionKHR = ^VkPresentRegionKHR;
     VkPresentRegionKHR = record
       rectangleCount :T_uint32_t;
       pRectangles :P_VkRectLayerKHR;
     end;

type P_VkPresentRegionsKHR = ^VkPresentRegionsKHR;
     VkPresentRegionsKHR = record
       sType :VkStructureType;
       pNext :P_void;
       swapchainCount :T_uint32_t;
       pRegions :P_VkPresentRegionKHR;
     end;



const VK_KHR_descriptor_update_template = 1;
type P_VkDescriptorUpdateTemplateKHR = ^VkDescriptorUpdateTemplateKHR;
     VkDescriptorUpdateTemplateKHR = VkDescriptorUpdateTemplate;

const VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1;
const VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = 'VK_KHR_descriptor_update_template';
type P_VkDescriptorUpdateTemplateTypeKHR = ^VkDescriptorUpdateTemplateTypeKHR;
     VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateType;

type P_VkDescriptorUpdateTemplateCreateFlagsKHR = ^VkDescriptorUpdateTemplateCreateFlagsKHR;
     VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlags;

type P_VkDescriptorUpdateTemplateEntryKHR = ^VkDescriptorUpdateTemplateEntryKHR;
     VkDescriptorUpdateTemplateEntryKHR = VkDescriptorUpdateTemplateEntry;

type P_VkDescriptorUpdateTemplateCreateInfoKHR = ^VkDescriptorUpdateTemplateCreateInfoKHR;
     VkDescriptorUpdateTemplateCreateInfoKHR = VkDescriptorUpdateTemplateCreateInfo;

type PFN_vkCreateDescriptorUpdateTemplateKHR = function( device_:VkDevice; const pCreateInfo_:P_VkDescriptorUpdateTemplateCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pDescriptorUpdateTemplate_:P_VkDescriptorUpdateTemplate ) :VkResult;
type PFN_vkDestroyDescriptorUpdateTemplateKHR = procedure( device_:VkDevice; descriptorUpdateTemplate_:VkDescriptorUpdateTemplate; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkUpdateDescriptorSetWithTemplateKHR = procedure( device_:VkDevice; descriptorSet_:VkDescriptorSet; descriptorUpdateTemplate_:VkDescriptorUpdateTemplate; const pData_:P_void );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateDescriptorUpdateTemplateKHR(
    device_:VkDevice;
    pCreateInfo_:P_VkDescriptorUpdateTemplateCreateInfo;
    pAllocator_:P_VkAllocationCallbacks;
    pDescriptorUpdateTemplate_:P_VkDescriptorUpdateTemplate ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDescriptorUpdateTemplateKHR(
    device_:VkDevice;
    descriptorUpdateTemplate_:VkDescriptorUpdateTemplate;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkUpdateDescriptorSetWithTemplateKHR(
    device_:VkDevice;
    descriptorSet_:VkDescriptorSet;
    descriptorUpdateTemplate_:VkDescriptorUpdateTemplate;
    pData_:P_void ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_imageless_framebuffer = 1;
const VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION = 1;
const VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME = 'VK_KHR_imageless_framebuffer';
type P_VkPhysicalDeviceImagelessFramebufferFeaturesKHR = ^VkPhysicalDeviceImagelessFramebufferFeaturesKHR;
     VkPhysicalDeviceImagelessFramebufferFeaturesKHR = VkPhysicalDeviceImagelessFramebufferFeatures;

type P_VkFramebufferAttachmentsCreateInfoKHR = ^VkFramebufferAttachmentsCreateInfoKHR;
     VkFramebufferAttachmentsCreateInfoKHR = VkFramebufferAttachmentsCreateInfo;

type P_VkFramebufferAttachmentImageInfoKHR = ^VkFramebufferAttachmentImageInfoKHR;
     VkFramebufferAttachmentImageInfoKHR = VkFramebufferAttachmentImageInfo;

type P_VkRenderPassAttachmentBeginInfoKHR = ^VkRenderPassAttachmentBeginInfoKHR;
     VkRenderPassAttachmentBeginInfoKHR = VkRenderPassAttachmentBeginInfo;



const VK_KHR_create_renderpass2 = 1;
const VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION = 1;
const VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME = 'VK_KHR_create_renderpass2';
type P_VkRenderPassCreateInfo2KHR = ^VkRenderPassCreateInfo2KHR;
     VkRenderPassCreateInfo2KHR = VkRenderPassCreateInfo2;

type P_VkAttachmentDescription2KHR = ^VkAttachmentDescription2KHR;
     VkAttachmentDescription2KHR = VkAttachmentDescription2;

type P_VkAttachmentReference2KHR = ^VkAttachmentReference2KHR;
     VkAttachmentReference2KHR = VkAttachmentReference2;

type P_VkSubpassDescription2KHR = ^VkSubpassDescription2KHR;
     VkSubpassDescription2KHR = VkSubpassDescription2;

type P_VkSubpassDependency2KHR = ^VkSubpassDependency2KHR;
     VkSubpassDependency2KHR = VkSubpassDependency2;

type P_VkSubpassBeginInfoKHR = ^VkSubpassBeginInfoKHR;
     VkSubpassBeginInfoKHR = VkSubpassBeginInfo;

type P_VkSubpassEndInfoKHR = ^VkSubpassEndInfoKHR;
     VkSubpassEndInfoKHR = VkSubpassEndInfo;

type PFN_vkCreateRenderPass2KHR = function( device_:VkDevice; const pCreateInfo_:P_VkRenderPassCreateInfo2; const pAllocator_:P_VkAllocationCallbacks; pRenderPass_:P_VkRenderPass ) :VkResult;
type PFN_vkCmdBeginRenderPass2KHR = procedure( commandBuffer_:VkCommandBuffer; const pRenderPassBegin_:P_VkRenderPassBeginInfo; const pSubpassBeginInfo_:P_VkSubpassBeginInfo );
type PFN_vkCmdNextSubpass2KHR = procedure( commandBuffer_:VkCommandBuffer; const pSubpassBeginInfo_:P_VkSubpassBeginInfo; const pSubpassEndInfo_:P_VkSubpassEndInfo );
type PFN_vkCmdEndRenderPass2KHR = procedure( commandBuffer_:VkCommandBuffer; const pSubpassEndInfo_:P_VkSubpassEndInfo );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateRenderPass2KHR(
    device_:VkDevice;
    pCreateInfo_:P_VkRenderPassCreateInfo2;
    pAllocator_:P_VkAllocationCallbacks;
    pRenderPass_:P_VkRenderPass ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdBeginRenderPass2KHR(
    commandBuffer_:VkCommandBuffer;
    pRenderPassBegin_:P_VkRenderPassBeginInfo;
    pSubpassBeginInfo_:P_VkSubpassBeginInfo ); stdcall; external DLLNAME;

procedure vkCmdNextSubpass2KHR(
    commandBuffer_:VkCommandBuffer;
    pSubpassBeginInfo_:P_VkSubpassBeginInfo;
    pSubpassEndInfo_:P_VkSubpassEndInfo ); stdcall; external DLLNAME;

procedure vkCmdEndRenderPass2KHR(
    commandBuffer_:VkCommandBuffer;
    pSubpassEndInfo_:P_VkSubpassEndInfo ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_shared_presentable_image = 1;
const VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1;
const VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = 'VK_KHR_shared_presentable_image';
type P_VkSharedPresentSurfaceCapabilitiesKHR = ^VkSharedPresentSurfaceCapabilitiesKHR;
     VkSharedPresentSurfaceCapabilitiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       sharedPresentSupportedUsageFlags :VkImageUsageFlags;
     end;

type PFN_vkGetSwapchainStatusKHR = function( device_:VkDevice; swapchain_:VkSwapchainKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetSwapchainStatusKHR(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_fence_capabilities = 1;
const VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME = 'VK_KHR_external_fence_capabilities';
type P_VkExternalFenceHandleTypeFlagsKHR = ^VkExternalFenceHandleTypeFlagsKHR;
     VkExternalFenceHandleTypeFlagsKHR = VkExternalFenceHandleTypeFlags;

type P_VkExternalFenceHandleTypeFlagBitsKHR = ^VkExternalFenceHandleTypeFlagBitsKHR;
     VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBits;

type P_VkExternalFenceFeatureFlagsKHR = ^VkExternalFenceFeatureFlagsKHR;
     VkExternalFenceFeatureFlagsKHR = VkExternalFenceFeatureFlags;

type P_VkExternalFenceFeatureFlagBitsKHR = ^VkExternalFenceFeatureFlagBitsKHR;
     VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBits;

type P_VkPhysicalDeviceExternalFenceInfoKHR = ^VkPhysicalDeviceExternalFenceInfoKHR;
     VkPhysicalDeviceExternalFenceInfoKHR = VkPhysicalDeviceExternalFenceInfo;

type P_VkExternalFencePropertiesKHR = ^VkExternalFencePropertiesKHR;
     VkExternalFencePropertiesKHR = VkExternalFenceProperties;

type PFN_vkGetPhysicalDeviceExternalFencePropertiesKHR = procedure( physicalDevice_:VkPhysicalDevice; const pExternalFenceInfo_:P_VkPhysicalDeviceExternalFenceInfo; pExternalFenceProperties_:P_VkExternalFenceProperties );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetPhysicalDeviceExternalFencePropertiesKHR(
    physicalDevice_:VkPhysicalDevice;
    pExternalFenceInfo_:P_VkPhysicalDeviceExternalFenceInfo;
    pExternalFenceProperties_:P_VkExternalFenceProperties ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_fence = 1;
const VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME = 'VK_KHR_external_fence';
type P_VkFenceImportFlagsKHR = ^VkFenceImportFlagsKHR;
     VkFenceImportFlagsKHR = VkFenceImportFlags;

type P_VkFenceImportFlagBitsKHR = ^VkFenceImportFlagBitsKHR;
     VkFenceImportFlagBitsKHR = VkFenceImportFlagBits;

type P_VkExportFenceCreateInfoKHR = ^VkExportFenceCreateInfoKHR;
     VkExportFenceCreateInfoKHR = VkExportFenceCreateInfo;



const VK_KHR_external_fence_fd = 1;
const VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1;
const VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = 'VK_KHR_external_fence_fd';
type P_VkImportFenceFdInfoKHR = ^VkImportFenceFdInfoKHR;
     VkImportFenceFdInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       fence :VkFence;
       flags :VkFenceImportFlags;
       handleType :VkExternalFenceHandleTypeFlagBits;
       fd :T_int;
     end;

type P_VkFenceGetFdInfoKHR = ^VkFenceGetFdInfoKHR;
     VkFenceGetFdInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       fence :VkFence;
       handleType :VkExternalFenceHandleTypeFlagBits;
     end;

type PFN_vkImportFenceFdKHR = function( device_:VkDevice; const pImportFenceFdInfo_:P_VkImportFenceFdInfoKHR ) :VkResult;
type PFN_vkGetFenceFdKHR = function( device_:VkDevice; const pGetFdInfo_:P_VkFenceGetFdInfoKHR; pFd_:P_int ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkImportFenceFdKHR(
    device_:VkDevice;
    pImportFenceFdInfo_:P_VkImportFenceFdInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetFenceFdKHR(
    device_:VkDevice;
    pGetFdInfo_:P_VkFenceGetFdInfoKHR;
    pFd_:P_int ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_performance_query = 1;
const VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1;
const VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME = 'VK_KHR_performance_query';

type P_VkPerformanceCounterUnitKHR = ^VkPerformanceCounterUnitKHR;
     VkPerformanceCounterUnitKHR = (
       VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR = 0,
       VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR = 1,
       VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR = 2,
       VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR = 3,
       VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR = 4,
       VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR = 5,
       VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR = 6,
       VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR = 7,
       VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR = 8,
       VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR = 9,
       VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR = 10,
       VK_PERFORMANCE_COUNTER_UNIT_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkPerformanceCounterScopeKHR = ^VkPerformanceCounterScopeKHR;
     VkPerformanceCounterScopeKHR = (
       VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR = 0,
       VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR = 1,
       VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR = 2,
       VK_QUERY_SCOPE_COMMAND_BUFFER_KHR = VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR,
       VK_QUERY_SCOPE_RENDER_PASS_KHR = VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR,
       VK_QUERY_SCOPE_COMMAND_KHR = VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR,
       VK_PERFORMANCE_COUNTER_SCOPE_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkPerformanceCounterStorageKHR = ^VkPerformanceCounterStorageKHR;
     VkPerformanceCounterStorageKHR = (
       VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR = 0,
       VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR = 1,
       VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR = 2,
       VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR = 3,
       VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR = 4,
       VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR = 5,
       VK_PERFORMANCE_COUNTER_STORAGE_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkPerformanceCounterDescriptionFlagBitsKHR = ^VkPerformanceCounterDescriptionFlagBitsKHR;
     VkPerformanceCounterDescriptionFlagBitsKHR = (
       VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR = $00000001,
       VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR = $00000002,
       VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR = VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR,
       VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR = VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR,
       VK_PERFORMANCE_COUNTER_DESCRIPTION_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkPerformanceCounterDescriptionFlagsKHR = ^VkPerformanceCounterDescriptionFlagsKHR;
     VkPerformanceCounterDescriptionFlagsKHR = VkFlags;

type P_VkAcquireProfilingLockFlagBitsKHR = ^VkAcquireProfilingLockFlagBitsKHR;
     VkAcquireProfilingLockFlagBitsKHR = (
       VK_ACQUIRE_PROFILING_LOCK_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkAcquireProfilingLockFlagsKHR = ^VkAcquireProfilingLockFlagsKHR;
     VkAcquireProfilingLockFlagsKHR = VkFlags;
type P_VkPhysicalDevicePerformanceQueryFeaturesKHR = ^VkPhysicalDevicePerformanceQueryFeaturesKHR;
     VkPhysicalDevicePerformanceQueryFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       performanceCounterQueryPools :VkBool32;
       performanceCounterMultipleQueryPools :VkBool32;
     end;

type P_VkPhysicalDevicePerformanceQueryPropertiesKHR = ^VkPhysicalDevicePerformanceQueryPropertiesKHR;
     VkPhysicalDevicePerformanceQueryPropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       allowCommandBufferQueryCopies :VkBool32;
     end;

type P_VkPerformanceCounterKHR = ^VkPerformanceCounterKHR;
     VkPerformanceCounterKHR = record
       sType :VkStructureType;
       pNext :P_void;
       unit_ :VkPerformanceCounterUnitKHR;
       scope :VkPerformanceCounterScopeKHR;
       storage :VkPerformanceCounterStorageKHR;
       uuid :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
     end;

type P_VkPerformanceCounterDescriptionKHR = ^VkPerformanceCounterDescriptionKHR;
     VkPerformanceCounterDescriptionKHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPerformanceCounterDescriptionFlagsKHR;
       name :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       category :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       description :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
     end;

type P_VkQueryPoolPerformanceCreateInfoKHR = ^VkQueryPoolPerformanceCreateInfoKHR;
     VkQueryPoolPerformanceCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       queueFamilyIndex :T_uint32_t;
       counterIndexCount :T_uint32_t;
       pCounterIndices :P_uint32_t;
     end;

type P_VkPerformanceCounterResultKHR = ^VkPerformanceCounterResultKHR;
     VkPerformanceCounterResultKHR = record
     case Byte of
       0:( int32   :T_int32_t  );
       1:( int64   :T_int64_t  );
       2:( uint32  :T_uint32_t );
       3:( uint64  :T_uint64_t );
       4:( float32 :T_float    );
       5:( float64 :double   );
     end;

type P_VkAcquireProfilingLockInfoKHR = ^VkAcquireProfilingLockInfoKHR;
     VkAcquireProfilingLockInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkAcquireProfilingLockFlagsKHR;
       timeout :T_uint64_t;
     end;

type P_VkPerformanceQuerySubmitInfoKHR = ^VkPerformanceQuerySubmitInfoKHR;
     VkPerformanceQuerySubmitInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       counterPassIndex :T_uint32_t;
     end;

type PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR = function( physicalDevice_:VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; pCounterCount_:P_uint32_t; pCounters_:P_VkPerformanceCounterKHR; pCounterDescriptions_:P_VkPerformanceCounterDescriptionKHR ) :VkResult;
type PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR = procedure( physicalDevice_:VkPhysicalDevice; const pPerformanceQueryCreateInfo_:P_VkQueryPoolPerformanceCreateInfoKHR; pNumPasses_:P_uint32_t );
type PFN_vkAcquireProfilingLockKHR = function( device_:VkDevice; const pInfo_:P_VkAcquireProfilingLockInfoKHR ) :VkResult;
type PFN_vkReleaseProfilingLockKHR = procedure( device_:VkDevice );

{$IFNDEF VK_NO_PROTOTYPES }
function vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR(
    physicalDevice_:VkPhysicalDevice;
    queueFamilyIndex_:T_uint32_t;
    pCounterCount_:P_uint32_t;
    pCounters_:P_VkPerformanceCounterKHR;
    pCounterDescriptions_:P_VkPerformanceCounterDescriptionKHR ) :VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR(
    physicalDevice_:VkPhysicalDevice;
    pPerformanceQueryCreateInfo_:P_VkQueryPoolPerformanceCreateInfoKHR;
    pNumPasses_:P_uint32_t ); stdcall; external DLLNAME;

function vkAcquireProfilingLockKHR(
    device_:VkDevice;
    pInfo_:P_VkAcquireProfilingLockInfoKHR ) :VkResult; stdcall; external DLLNAME;

procedure vkReleaseProfilingLockKHR(
    device_:VkDevice ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_maintenance2 = 1;
const VK_KHR_MAINTENANCE2_SPEC_VERSION  = 1;
const VK_KHR_MAINTENANCE2_EXTENSION_NAME = 'VK_KHR_maintenance2';
type P_VkPointClippingBehaviorKHR = ^VkPointClippingBehaviorKHR;
     VkPointClippingBehaviorKHR = VkPointClippingBehavior;

type P_VkTessellationDomainOriginKHR = ^VkTessellationDomainOriginKHR;
     VkTessellationDomainOriginKHR = VkTessellationDomainOrigin;

type P_VkPhysicalDevicePointClippingPropertiesKHR = ^VkPhysicalDevicePointClippingPropertiesKHR;
     VkPhysicalDevicePointClippingPropertiesKHR = VkPhysicalDevicePointClippingProperties;

type P_VkRenderPassInputAttachmentAspectCreateInfoKHR = ^VkRenderPassInputAttachmentAspectCreateInfoKHR;
     VkRenderPassInputAttachmentAspectCreateInfoKHR = VkRenderPassInputAttachmentAspectCreateInfo;

type P_VkInputAttachmentAspectReferenceKHR = ^VkInputAttachmentAspectReferenceKHR;
     VkInputAttachmentAspectReferenceKHR = VkInputAttachmentAspectReference;

type P_VkImageViewUsageCreateInfoKHR = ^VkImageViewUsageCreateInfoKHR;
     VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfo;

type P_VkPipelineTessellationDomainOriginStateCreateInfoKHR = ^VkPipelineTessellationDomainOriginStateCreateInfoKHR;
     VkPipelineTessellationDomainOriginStateCreateInfoKHR = VkPipelineTessellationDomainOriginStateCreateInfo;



const VK_KHR_get_surface_capabilities2 = 1;
const VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1;
const VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME = 'VK_KHR_get_surface_capabilities2';
type P_VkPhysicalDeviceSurfaceInfo2KHR = ^VkPhysicalDeviceSurfaceInfo2KHR;
     VkPhysicalDeviceSurfaceInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       surface :VkSurfaceKHR;
     end;

type P_VkSurfaceCapabilities2KHR = ^VkSurfaceCapabilities2KHR;
     VkSurfaceCapabilities2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       surfaceCapabilities :VkSurfaceCapabilitiesKHR;
     end;

type P_VkSurfaceFormat2KHR = ^VkSurfaceFormat2KHR;
     VkSurfaceFormat2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       surfaceFormat :VkSurfaceFormatKHR;
     end;

type PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR = function( physicalDevice_:VkPhysicalDevice; const pSurfaceInfo_:P_VkPhysicalDeviceSurfaceInfo2KHR; pSurfaceCapabilities_:P_VkSurfaceCapabilities2KHR ) :VkResult;
type PFN_vkGetPhysicalDeviceSurfaceFormats2KHR = function( physicalDevice_:VkPhysicalDevice; const pSurfaceInfo_:P_VkPhysicalDeviceSurfaceInfo2KHR; pSurfaceFormatCount_:P_uint32_t; pSurfaceFormats_:P_VkSurfaceFormat2KHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceSurfaceCapabilities2KHR(
    physicalDevice_:VkPhysicalDevice;
    pSurfaceInfo_:P_VkPhysicalDeviceSurfaceInfo2KHR;
    pSurfaceCapabilities_:P_VkSurfaceCapabilities2KHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceFormats2KHR(
    physicalDevice_:VkPhysicalDevice;
    pSurfaceInfo_:P_VkPhysicalDeviceSurfaceInfo2KHR;
    pSurfaceFormatCount_:P_uint32_t;
    pSurfaceFormats_:P_VkSurfaceFormat2KHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_variable_pointers = 1;
const VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1;
const VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME = 'VK_KHR_variable_pointers';
type P_VkPhysicalDeviceVariablePointerFeaturesKHR = ^VkPhysicalDeviceVariablePointerFeaturesKHR;
     VkPhysicalDeviceVariablePointerFeaturesKHR = VkPhysicalDeviceVariablePointersFeatures;

type P_VkPhysicalDeviceVariablePointersFeaturesKHR = ^VkPhysicalDeviceVariablePointersFeaturesKHR;
     VkPhysicalDeviceVariablePointersFeaturesKHR = VkPhysicalDeviceVariablePointersFeatures;



const VK_KHR_get_display_properties2 = 1;
const VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1;
const VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = 'VK_KHR_get_display_properties2';
type P_VkDisplayProperties2KHR = ^VkDisplayProperties2KHR;
     VkDisplayProperties2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       displayProperties :VkDisplayPropertiesKHR;
     end;

type P_VkDisplayPlaneProperties2KHR = ^VkDisplayPlaneProperties2KHR;
     VkDisplayPlaneProperties2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       displayPlaneProperties :VkDisplayPlanePropertiesKHR;
     end;

type P_VkDisplayModeProperties2KHR = ^VkDisplayModeProperties2KHR;
     VkDisplayModeProperties2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       displayModeProperties :VkDisplayModePropertiesKHR;
     end;

type P_VkDisplayPlaneInfo2KHR = ^VkDisplayPlaneInfo2KHR;
     VkDisplayPlaneInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       mode :VkDisplayModeKHR;
       planeIndex :T_uint32_t;
     end;

type P_VkDisplayPlaneCapabilities2KHR = ^VkDisplayPlaneCapabilities2KHR;
     VkDisplayPlaneCapabilities2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       capabilities :VkDisplayPlaneCapabilitiesKHR;
     end;

type PFN_vkGetPhysicalDeviceDisplayProperties2KHR = function( physicalDevice_:VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayProperties2KHR ) :VkResult;
type PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = function( physicalDevice_:VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayPlaneProperties2KHR ) :VkResult;
type PFN_vkGetDisplayModeProperties2KHR = function( physicalDevice_:VkPhysicalDevice; display_:VkDisplayKHR; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayModeProperties2KHR ) :VkResult;
type PFN_vkGetDisplayPlaneCapabilities2KHR = function( physicalDevice_:VkPhysicalDevice; const pDisplayPlaneInfo_:P_VkDisplayPlaneInfo2KHR; pCapabilities_:P_VkDisplayPlaneCapabilities2KHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceDisplayProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkDisplayProperties2KHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceDisplayPlaneProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkDisplayPlaneProperties2KHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDisplayModeProperties2KHR(
    physicalDevice_:VkPhysicalDevice;
    display_:VkDisplayKHR;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkDisplayModeProperties2KHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDisplayPlaneCapabilities2KHR(
    physicalDevice_:VkPhysicalDevice;
    pDisplayPlaneInfo_:P_VkDisplayPlaneInfo2KHR;
    pCapabilities_:P_VkDisplayPlaneCapabilities2KHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_dedicated_allocation = 1;
const VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3;
const VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = 'VK_KHR_dedicated_allocation';
type P_VkMemoryDedicatedRequirementsKHR = ^VkMemoryDedicatedRequirementsKHR;
     VkMemoryDedicatedRequirementsKHR = VkMemoryDedicatedRequirements;

type P_VkMemoryDedicatedAllocateInfoKHR = ^VkMemoryDedicatedAllocateInfoKHR;
     VkMemoryDedicatedAllocateInfoKHR = VkMemoryDedicatedAllocateInfo;



const VK_KHR_storage_buffer_storage_class = 1;
const VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1;
const VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME = 'VK_KHR_storage_buffer_storage_class';


const VK_KHR_relaxed_block_layout = 1;
const VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1;
const VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME = 'VK_KHR_relaxed_block_layout';


const VK_KHR_get_memory_requirements2 = 1;
const VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1;
const VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME = 'VK_KHR_get_memory_requirements2';
type P_VkBufferMemoryRequirementsInfo2KHR = ^VkBufferMemoryRequirementsInfo2KHR;
     VkBufferMemoryRequirementsInfo2KHR = VkBufferMemoryRequirementsInfo2;

type P_VkImageMemoryRequirementsInfo2KHR = ^VkImageMemoryRequirementsInfo2KHR;
     VkImageMemoryRequirementsInfo2KHR = VkImageMemoryRequirementsInfo2;

type P_VkImageSparseMemoryRequirementsInfo2KHR = ^VkImageSparseMemoryRequirementsInfo2KHR;
     VkImageSparseMemoryRequirementsInfo2KHR = VkImageSparseMemoryRequirementsInfo2;

type P_VkMemoryRequirements2KHR = ^VkMemoryRequirements2KHR;
     VkMemoryRequirements2KHR = VkMemoryRequirements2;

type P_VkSparseImageMemoryRequirements2KHR = ^VkSparseImageMemoryRequirements2KHR;
     VkSparseImageMemoryRequirements2KHR = VkSparseImageMemoryRequirements2;

type PFN_vkGetImageMemoryRequirements2KHR = procedure( device_:VkDevice; const pInfo_:P_VkImageMemoryRequirementsInfo2; pMemoryRequirements_:P_VkMemoryRequirements2 );
type PFN_vkGetBufferMemoryRequirements2KHR = procedure( device_:VkDevice; const pInfo_:P_VkBufferMemoryRequirementsInfo2; pMemoryRequirements_:P_VkMemoryRequirements2 );
type PFN_vkGetImageSparseMemoryRequirements2KHR = procedure( device_:VkDevice; const pInfo_:P_VkImageSparseMemoryRequirementsInfo2; pSparseMemoryRequirementCount_:P_uint32_t; pSparseMemoryRequirements_:P_VkSparseImageMemoryRequirements2 );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetImageMemoryRequirements2KHR(
    device_:VkDevice;
    pInfo_:P_VkImageMemoryRequirementsInfo2;
    pMemoryRequirements_:P_VkMemoryRequirements2 ); stdcall; external DLLNAME;

procedure vkGetBufferMemoryRequirements2KHR(
    device_:VkDevice;
    pInfo_:P_VkBufferMemoryRequirementsInfo2;
    pMemoryRequirements_:P_VkMemoryRequirements2 ); stdcall; external DLLNAME;

procedure vkGetImageSparseMemoryRequirements2KHR(
    device_:VkDevice;
    pInfo_:P_VkImageSparseMemoryRequirementsInfo2;
    pSparseMemoryRequirementCount_:P_uint32_t;
    pSparseMemoryRequirements_:P_VkSparseImageMemoryRequirements2 ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_image_format_list = 1;
const VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1;
const VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = 'VK_KHR_image_format_list';
type P_VkImageFormatListCreateInfoKHR = ^VkImageFormatListCreateInfoKHR;
     VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfo;



const VK_KHR_sampler_ycbcr_conversion = 1;
type P_VkSamplerYcbcrConversionKHR = ^VkSamplerYcbcrConversionKHR;
     VkSamplerYcbcrConversionKHR = VkSamplerYcbcrConversion;

const VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 14;
const VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME = 'VK_KHR_sampler_ycbcr_conversion';
type P_VkSamplerYcbcrModelConversionKHR = ^VkSamplerYcbcrModelConversionKHR;
     VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversion;

type P_VkSamplerYcbcrRangeKHR = ^VkSamplerYcbcrRangeKHR;
     VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRange;

type P_VkChromaLocationKHR = ^VkChromaLocationKHR;
     VkChromaLocationKHR = VkChromaLocation;

type P_VkSamplerYcbcrConversionCreateInfoKHR = ^VkSamplerYcbcrConversionCreateInfoKHR;
     VkSamplerYcbcrConversionCreateInfoKHR = VkSamplerYcbcrConversionCreateInfo;

type P_VkSamplerYcbcrConversionInfoKHR = ^VkSamplerYcbcrConversionInfoKHR;
     VkSamplerYcbcrConversionInfoKHR = VkSamplerYcbcrConversionInfo;

type P_VkBindImagePlaneMemoryInfoKHR = ^VkBindImagePlaneMemoryInfoKHR;
     VkBindImagePlaneMemoryInfoKHR = VkBindImagePlaneMemoryInfo;

type P_VkImagePlaneMemoryRequirementsInfoKHR = ^VkImagePlaneMemoryRequirementsInfoKHR;
     VkImagePlaneMemoryRequirementsInfoKHR = VkImagePlaneMemoryRequirementsInfo;

type P_VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR = ^VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR;
     VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR = VkPhysicalDeviceSamplerYcbcrConversionFeatures;

type P_VkSamplerYcbcrConversionImageFormatPropertiesKHR = ^VkSamplerYcbcrConversionImageFormatPropertiesKHR;
     VkSamplerYcbcrConversionImageFormatPropertiesKHR = VkSamplerYcbcrConversionImageFormatProperties;

type PFN_vkCreateSamplerYcbcrConversionKHR = function( device_:VkDevice; const pCreateInfo_:P_VkSamplerYcbcrConversionCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pYcbcrConversion_:P_VkSamplerYcbcrConversion ) :VkResult;
type PFN_vkDestroySamplerYcbcrConversionKHR = procedure( device_:VkDevice; ycbcrConversion_:VkSamplerYcbcrConversion; const pAllocator_:P_VkAllocationCallbacks );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateSamplerYcbcrConversionKHR(
    device_:VkDevice;
    pCreateInfo_:P_VkSamplerYcbcrConversionCreateInfo;
    pAllocator_:P_VkAllocationCallbacks;
    pYcbcrConversion_:P_VkSamplerYcbcrConversion ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroySamplerYcbcrConversionKHR(
    device_:VkDevice;
    ycbcrConversion_:VkSamplerYcbcrConversion;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_bind_memory2 = 1;
const VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1;
const VK_KHR_BIND_MEMORY_2_EXTENSION_NAME = 'VK_KHR_bind_memory2';
type P_VkBindBufferMemoryInfoKHR = ^VkBindBufferMemoryInfoKHR;
     VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfo;

type P_VkBindImageMemoryInfoKHR = ^VkBindImageMemoryInfoKHR;
     VkBindImageMemoryInfoKHR = VkBindImageMemoryInfo;

type PFN_vkBindBufferMemory2KHR = function( device_:VkDevice; bindInfoCount_:T_uint32_t; const pBindInfos_:P_VkBindBufferMemoryInfo ) :VkResult;
type PFN_vkBindImageMemory2KHR = function( device_:VkDevice; bindInfoCount_:T_uint32_t; const pBindInfos_:P_VkBindImageMemoryInfo ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkBindBufferMemory2KHR(
    device_:VkDevice;
    bindInfoCount_:T_uint32_t;
    pBindInfos_:P_VkBindBufferMemoryInfo ) :VkResult; stdcall; external DLLNAME;

function vkBindImageMemory2KHR(
    device_:VkDevice;
    bindInfoCount_:T_uint32_t;
    pBindInfos_:P_VkBindImageMemoryInfo ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_maintenance3 = 1;
const VK_KHR_MAINTENANCE3_SPEC_VERSION  = 1;
const VK_KHR_MAINTENANCE3_EXTENSION_NAME = 'VK_KHR_maintenance3';
type P_VkPhysicalDeviceMaintenance3PropertiesKHR = ^VkPhysicalDeviceMaintenance3PropertiesKHR;
     VkPhysicalDeviceMaintenance3PropertiesKHR = VkPhysicalDeviceMaintenance3Properties;

type P_VkDescriptorSetLayoutSupportKHR = ^VkDescriptorSetLayoutSupportKHR;
     VkDescriptorSetLayoutSupportKHR = VkDescriptorSetLayoutSupport;

type PFN_vkGetDescriptorSetLayoutSupportKHR = procedure( device_:VkDevice; const pCreateInfo_:P_VkDescriptorSetLayoutCreateInfo; pSupport_:P_VkDescriptorSetLayoutSupport );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetDescriptorSetLayoutSupportKHR(
    device_:VkDevice;
    pCreateInfo_:P_VkDescriptorSetLayoutCreateInfo;
    pSupport_:P_VkDescriptorSetLayoutSupport ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_draw_indirect_count = 1;
const VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1;
const VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = 'VK_KHR_draw_indirect_count';
type PFN_vkCmdDrawIndirectCountKHR = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCmdDrawIndexedIndirectCountKHR = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdDrawIndirectCountKHR(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexedIndirectCountKHR(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_shader_subgroup_extended_types = 1;
const VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION = 1;
const VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME = 'VK_KHR_shader_subgroup_extended_types';
type P_VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR = ^VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR;
     VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR = VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures;



const VK_KHR_8bit_storage = 1;
const VK_KHR_8BIT_STORAGE_SPEC_VERSION  = 1;
const VK_KHR_8BIT_STORAGE_EXTENSION_NAME = 'VK_KHR_8bit_storage';
type P_VkPhysicalDevice8BitStorageFeaturesKHR = ^VkPhysicalDevice8BitStorageFeaturesKHR;
     VkPhysicalDevice8BitStorageFeaturesKHR = VkPhysicalDevice8BitStorageFeatures;



const VK_KHR_shader_atomic_int64 = 1;
const VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = 1;
const VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME = 'VK_KHR_shader_atomic_int64';
type P_VkPhysicalDeviceShaderAtomicInt64FeaturesKHR = ^VkPhysicalDeviceShaderAtomicInt64FeaturesKHR;
     VkPhysicalDeviceShaderAtomicInt64FeaturesKHR = VkPhysicalDeviceShaderAtomicInt64Features;



const VK_KHR_shader_clock = 1;
const VK_KHR_SHADER_CLOCK_SPEC_VERSION  = 1;
const VK_KHR_SHADER_CLOCK_EXTENSION_NAME = 'VK_KHR_shader_clock';
type P_VkPhysicalDeviceShaderClockFeaturesKHR = ^VkPhysicalDeviceShaderClockFeaturesKHR;
     VkPhysicalDeviceShaderClockFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       shaderSubgroupClock :VkBool32;
       shaderDeviceClock :VkBool32;
     end;



const VK_KHR_driver_properties = 1;
const VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION = 1;
const VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME = 'VK_KHR_driver_properties';
const VK_MAX_DRIVER_NAME_SIZE_KHR       = VK_MAX_DRIVER_NAME_SIZE;
const VK_MAX_DRIVER_INFO_SIZE_KHR       = VK_MAX_DRIVER_INFO_SIZE;
type P_VkDriverIdKHR = ^VkDriverIdKHR;
     VkDriverIdKHR = VkDriverId;

type P_VkConformanceVersionKHR = ^VkConformanceVersionKHR;
     VkConformanceVersionKHR = VkConformanceVersion;

type P_VkPhysicalDeviceDriverPropertiesKHR = ^VkPhysicalDeviceDriverPropertiesKHR;
     VkPhysicalDeviceDriverPropertiesKHR = VkPhysicalDeviceDriverProperties;



const VK_KHR_shader_float_controls = 1;
const VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 4;
const VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME = 'VK_KHR_shader_float_controls';
type P_VkShaderFloatControlsIndependenceKHR = ^VkShaderFloatControlsIndependenceKHR;
     VkShaderFloatControlsIndependenceKHR = VkShaderFloatControlsIndependence;

type P_VkPhysicalDeviceFloatControlsPropertiesKHR = ^VkPhysicalDeviceFloatControlsPropertiesKHR;
     VkPhysicalDeviceFloatControlsPropertiesKHR = VkPhysicalDeviceFloatControlsProperties;



const VK_KHR_depth_stencil_resolve = 1;
const VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1;
const VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME = 'VK_KHR_depth_stencil_resolve';
type P_VkResolveModeFlagBitsKHR = ^VkResolveModeFlagBitsKHR;
     VkResolveModeFlagBitsKHR = VkResolveModeFlagBits;

type P_VkResolveModeFlagsKHR = ^VkResolveModeFlagsKHR;
     VkResolveModeFlagsKHR = VkResolveModeFlags;

type P_VkSubpassDescriptionDepthStencilResolveKHR = ^VkSubpassDescriptionDepthStencilResolveKHR;
     VkSubpassDescriptionDepthStencilResolveKHR = VkSubpassDescriptionDepthStencilResolve;

type P_VkPhysicalDeviceDepthStencilResolvePropertiesKHR = ^VkPhysicalDeviceDepthStencilResolvePropertiesKHR;
     VkPhysicalDeviceDepthStencilResolvePropertiesKHR = VkPhysicalDeviceDepthStencilResolveProperties;



const VK_KHR_swapchain_mutable_format = 1;
const VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1;
const VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = 'VK_KHR_swapchain_mutable_format';


const VK_KHR_timeline_semaphore = 1;
const VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2;
const VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME = 'VK_KHR_timeline_semaphore';
type P_VkSemaphoreTypeKHR = ^VkSemaphoreTypeKHR;
     VkSemaphoreTypeKHR = VkSemaphoreType;

type P_VkSemaphoreWaitFlagBitsKHR = ^VkSemaphoreWaitFlagBitsKHR;
     VkSemaphoreWaitFlagBitsKHR = VkSemaphoreWaitFlagBits;

type P_VkSemaphoreWaitFlagsKHR = ^VkSemaphoreWaitFlagsKHR;
     VkSemaphoreWaitFlagsKHR = VkSemaphoreWaitFlags;

type P_VkPhysicalDeviceTimelineSemaphoreFeaturesKHR = ^VkPhysicalDeviceTimelineSemaphoreFeaturesKHR;
     VkPhysicalDeviceTimelineSemaphoreFeaturesKHR = VkPhysicalDeviceTimelineSemaphoreFeatures;

type P_VkPhysicalDeviceTimelineSemaphorePropertiesKHR = ^VkPhysicalDeviceTimelineSemaphorePropertiesKHR;
     VkPhysicalDeviceTimelineSemaphorePropertiesKHR = VkPhysicalDeviceTimelineSemaphoreProperties;

type P_VkSemaphoreTypeCreateInfoKHR = ^VkSemaphoreTypeCreateInfoKHR;
     VkSemaphoreTypeCreateInfoKHR = VkSemaphoreTypeCreateInfo;

type P_VkTimelineSemaphoreSubmitInfoKHR = ^VkTimelineSemaphoreSubmitInfoKHR;
     VkTimelineSemaphoreSubmitInfoKHR = VkTimelineSemaphoreSubmitInfo;

type P_VkSemaphoreWaitInfoKHR = ^VkSemaphoreWaitInfoKHR;
     VkSemaphoreWaitInfoKHR = VkSemaphoreWaitInfo;

type P_VkSemaphoreSignalInfoKHR = ^VkSemaphoreSignalInfoKHR;
     VkSemaphoreSignalInfoKHR = VkSemaphoreSignalInfo;

type PFN_vkGetSemaphoreCounterValueKHR = function( device_:VkDevice; semaphore_:VkSemaphore; pValue_:P_uint64_t ) :VkResult;
type PFN_vkWaitSemaphoresKHR = function( device_:VkDevice; const pWaitInfo_:P_VkSemaphoreWaitInfo; timeout_:T_uint64_t ) :VkResult;
type PFN_vkSignalSemaphoreKHR = function( device_:VkDevice; const pSignalInfo_:P_VkSemaphoreSignalInfo ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetSemaphoreCounterValueKHR(
    device_:VkDevice;
    semaphore_:VkSemaphore;
    pValue_:P_uint64_t ) :VkResult; stdcall; external DLLNAME;

function vkWaitSemaphoresKHR(
    device_:VkDevice;
    pWaitInfo_:P_VkSemaphoreWaitInfo;
    timeout_:T_uint64_t ) :VkResult; stdcall; external DLLNAME;

function vkSignalSemaphoreKHR(
    device_:VkDevice;
    pSignalInfo_:P_VkSemaphoreSignalInfo ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_vulkan_memory_model = 1;
const VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3;
const VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = 'VK_KHR_vulkan_memory_model';
type P_VkPhysicalDeviceVulkanMemoryModelFeaturesKHR = ^VkPhysicalDeviceVulkanMemoryModelFeaturesKHR;
     VkPhysicalDeviceVulkanMemoryModelFeaturesKHR = VkPhysicalDeviceVulkanMemoryModelFeatures;



const VK_KHR_shader_terminate_invocation = 1;
const VK_KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION = 1;
const VK_KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME = 'VK_KHR_shader_terminate_invocation';
type P_VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR = ^VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR;
     VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       shaderTerminateInvocation :VkBool32;
     end;



const VK_KHR_fragment_shading_rate = 1;
const VK_KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1;
const VK_KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = 'VK_KHR_fragment_shading_rate';

type P_VkFragmentShadingRateCombinerOpKHR = ^VkFragmentShadingRateCombinerOpKHR;
     VkFragmentShadingRateCombinerOpKHR = (
       VK_FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR = 0,
       VK_FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR = 1,
       VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR = 2,
       VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR = 3,
       VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR = 4,
       VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkFragmentShadingRateAttachmentInfoKHR = ^VkFragmentShadingRateAttachmentInfoKHR;
     VkFragmentShadingRateAttachmentInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       pFragmentShadingRateAttachment :P_VkAttachmentReference2;
       shadingRateAttachmentTexelSize :VkExtent2D;
     end;

type P_VkPipelineFragmentShadingRateStateCreateInfoKHR = ^VkPipelineFragmentShadingRateStateCreateInfoKHR;
     VkPipelineFragmentShadingRateStateCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentSize :VkExtent2D;
       combinerOps :array [ 0..2-1 ] of VkFragmentShadingRateCombinerOpKHR;
     end;

type P_VkPhysicalDeviceFragmentShadingRateFeaturesKHR = ^VkPhysicalDeviceFragmentShadingRateFeaturesKHR;
     VkPhysicalDeviceFragmentShadingRateFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       pipelineFragmentShadingRate :VkBool32;
       primitiveFragmentShadingRate :VkBool32;
       attachmentFragmentShadingRate :VkBool32;
     end;

type P_VkPhysicalDeviceFragmentShadingRatePropertiesKHR = ^VkPhysicalDeviceFragmentShadingRatePropertiesKHR;
     VkPhysicalDeviceFragmentShadingRatePropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       minFragmentShadingRateAttachmentTexelSize :VkExtent2D;
       maxFragmentShadingRateAttachmentTexelSize :VkExtent2D;
       maxFragmentShadingRateAttachmentTexelSizeAspectRatio :T_uint32_t;
       primitiveFragmentShadingRateWithMultipleViewports :VkBool32;
       layeredShadingRateAttachments :VkBool32;
       fragmentShadingRateNonTrivialCombinerOps :VkBool32;
       maxFragmentSize :VkExtent2D;
       maxFragmentSizeAspectRatio :T_uint32_t;
       maxFragmentShadingRateCoverageSamples :T_uint32_t;
       maxFragmentShadingRateRasterizationSamples :VkSampleCountFlagBits;
       fragmentShadingRateWithShaderDepthStencilWrites :VkBool32;
       fragmentShadingRateWithSampleMask :VkBool32;
       fragmentShadingRateWithShaderSampleMask :VkBool32;
       fragmentShadingRateWithConservativeRasterization :VkBool32;
       fragmentShadingRateWithFragmentShaderInterlock :VkBool32;
       fragmentShadingRateWithCustomSampleLocations :VkBool32;
       fragmentShadingRateStrictMultiplyCombiner :VkBool32;
     end;

type P_VkPhysicalDeviceFragmentShadingRateKHR = ^VkPhysicalDeviceFragmentShadingRateKHR;
     VkPhysicalDeviceFragmentShadingRateKHR = record
       sType :VkStructureType;
       pNext :P_void;
       sampleCounts :VkSampleCountFlags;
       fragmentSize :VkExtent2D;
     end;

type PFN_vkGetPhysicalDeviceFragmentShadingRatesKHR = function( physicalDevice_:VkPhysicalDevice; pFragmentShadingRateCount_:P_uint32_t; pFragmentShadingRates_:P_VkPhysicalDeviceFragmentShadingRateKHR ) :VkResult;
type T_combinerOps = array [ 0..2-1 ] of VkFragmentShadingRateCombinerOpKHR;
     PFN_vkCmdSetFragmentShadingRateKHR = procedure( commandBuffer_:VkCommandBuffer; const pFragmentSize_:P_VkExtent2D; const combinerOps_:T_combinerOps );

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceFragmentShadingRatesKHR(
    physicalDevice_:VkPhysicalDevice;
    pFragmentShadingRateCount_:P_uint32_t;
    pFragmentShadingRates_:P_VkPhysicalDeviceFragmentShadingRateKHR ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdSetFragmentShadingRateKHR(
    commandBuffer_:VkCommandBuffer;
    pFragmentSize_:P_VkExtent2D;
    const combinerOps_:T_combinerOps ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_spirv_1_4 = 1;
const VK_KHR_SPIRV_1_4_SPEC_VERSION     = 1;
const VK_KHR_SPIRV_1_4_EXTENSION_NAME = 'VK_KHR_spirv_1_4';


const VK_KHR_surface_protected_capabilities = 1;
const VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1;
const VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME = 'VK_KHR_surface_protected_capabilities';
type P_VkSurfaceProtectedCapabilitiesKHR = ^VkSurfaceProtectedCapabilitiesKHR;
     VkSurfaceProtectedCapabilitiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       supportsProtected :VkBool32;
     end;



const VK_KHR_separate_depth_stencil_layouts = 1;
const VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION = 1;
const VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME = 'VK_KHR_separate_depth_stencil_layouts';
type P_VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR = ^VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR;
     VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR = VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures;

type P_VkAttachmentReferenceStencilLayoutKHR = ^VkAttachmentReferenceStencilLayoutKHR;
     VkAttachmentReferenceStencilLayoutKHR = VkAttachmentReferenceStencilLayout;

type P_VkAttachmentDescriptionStencilLayoutKHR = ^VkAttachmentDescriptionStencilLayoutKHR;
     VkAttachmentDescriptionStencilLayoutKHR = VkAttachmentDescriptionStencilLayout;



const VK_KHR_uniform_buffer_standard_layout = 1;
const VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1;
const VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME = 'VK_KHR_uniform_buffer_standard_layout';
type P_VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR = ^VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR;
     VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR = VkPhysicalDeviceUniformBufferStandardLayoutFeatures;



const VK_KHR_buffer_device_address = 1;
const VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 1;
const VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = 'VK_KHR_buffer_device_address';
type P_VkPhysicalDeviceBufferDeviceAddressFeaturesKHR = ^VkPhysicalDeviceBufferDeviceAddressFeaturesKHR;
     VkPhysicalDeviceBufferDeviceAddressFeaturesKHR = VkPhysicalDeviceBufferDeviceAddressFeatures;

type P_VkBufferDeviceAddressInfoKHR = ^VkBufferDeviceAddressInfoKHR;
     VkBufferDeviceAddressInfoKHR = VkBufferDeviceAddressInfo;

type P_VkBufferOpaqueCaptureAddressCreateInfoKHR = ^VkBufferOpaqueCaptureAddressCreateInfoKHR;
     VkBufferOpaqueCaptureAddressCreateInfoKHR = VkBufferOpaqueCaptureAddressCreateInfo;

type P_VkMemoryOpaqueCaptureAddressAllocateInfoKHR = ^VkMemoryOpaqueCaptureAddressAllocateInfoKHR;
     VkMemoryOpaqueCaptureAddressAllocateInfoKHR = VkMemoryOpaqueCaptureAddressAllocateInfo;

type P_VkDeviceMemoryOpaqueCaptureAddressInfoKHR = ^VkDeviceMemoryOpaqueCaptureAddressInfoKHR;
     VkDeviceMemoryOpaqueCaptureAddressInfoKHR = VkDeviceMemoryOpaqueCaptureAddressInfo;

type PFN_vkGetBufferDeviceAddressKHR = function( device_:VkDevice; const pInfo_:P_VkBufferDeviceAddressInfo ) :VkDeviceAddress;
type PFN_vkGetBufferOpaqueCaptureAddressKHR = function( device_:VkDevice; const pInfo_:P_VkBufferDeviceAddressInfo ) :T_uint64_t;
type PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR = function( device_:VkDevice; const pInfo_:P_VkDeviceMemoryOpaqueCaptureAddressInfo ) :T_uint64_t;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetBufferDeviceAddressKHR(
    device_:VkDevice;
    pInfo_:P_VkBufferDeviceAddressInfo ) :VkDeviceAddress; stdcall; external DLLNAME;

function vkGetBufferOpaqueCaptureAddressKHR(
    device_:VkDevice;
    pInfo_:P_VkBufferDeviceAddressInfo ) :T_uint64_t; stdcall; external DLLNAME;

function vkGetDeviceMemoryOpaqueCaptureAddressKHR(
    device_:VkDevice;
    pInfo_:P_VkDeviceMemoryOpaqueCaptureAddressInfo ) :T_uint64_t; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_deferred_host_operations = 1;
type P_VkDeferredOperationKHR = ^VkDeferredOperationKHR;
     VkDeferredOperationKHR = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 4;
const VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME = 'VK_KHR_deferred_host_operations';
type PFN_vkCreateDeferredOperationKHR = function( device_:VkDevice; const pAllocator_:P_VkAllocationCallbacks; pDeferredOperation_:P_VkDeferredOperationKHR ) :VkResult;
type PFN_vkDestroyDeferredOperationKHR = procedure( device_:VkDevice; operation_:VkDeferredOperationKHR; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetDeferredOperationMaxConcurrencyKHR = function( device_:VkDevice; operation_:VkDeferredOperationKHR ) :T_uint32_t;
type PFN_vkGetDeferredOperationResultKHR = function( device_:VkDevice; operation_:VkDeferredOperationKHR ) :VkResult;
type PFN_vkDeferredOperationJoinKHR = function( device_:VkDevice; operation_:VkDeferredOperationKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateDeferredOperationKHR(
    device_:VkDevice;
    pAllocator_:P_VkAllocationCallbacks;
    pDeferredOperation_:P_VkDeferredOperationKHR ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDeferredOperationKHR(
    device_:VkDevice;
    operation_:VkDeferredOperationKHR;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetDeferredOperationMaxConcurrencyKHR(
    device_:VkDevice;
    operation_:VkDeferredOperationKHR ) :T_uint32_t; stdcall; external DLLNAME;

function vkGetDeferredOperationResultKHR(
    device_:VkDevice;
    operation_:VkDeferredOperationKHR ) :VkResult; stdcall; external DLLNAME;

function vkDeferredOperationJoinKHR(
    device_:VkDevice;
    operation_:VkDeferredOperationKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_pipeline_executable_properties = 1;
const VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1;
const VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = 'VK_KHR_pipeline_executable_properties';

type P_VkPipelineExecutableStatisticFormatKHR = ^VkPipelineExecutableStatisticFormatKHR;
     VkPipelineExecutableStatisticFormatKHR = (
       VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR = 0,
       VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR = 1,
       VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR = 2,
       VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR = 3,
       VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR = ^VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR;
     VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       pipelineExecutableInfo :VkBool32;
     end;

type P_VkPipelineInfoKHR = ^VkPipelineInfoKHR;
     VkPipelineInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       pipeline :VkPipeline;
     end;

type P_VkPipelineExecutablePropertiesKHR = ^VkPipelineExecutablePropertiesKHR;
     VkPipelineExecutablePropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       stages :VkShaderStageFlags;
       name :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       description :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       subgroupSize :T_uint32_t;
     end;

type P_VkPipelineExecutableInfoKHR = ^VkPipelineExecutableInfoKHR;
     VkPipelineExecutableInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       pipeline :VkPipeline;
       executableIndex :T_uint32_t;
     end;

type P_VkPipelineExecutableStatisticValueKHR = ^VkPipelineExecutableStatisticValueKHR;
     VkPipelineExecutableStatisticValueKHR = record
     case Byte of
       0:( b32 :VkBool32 );
       1:( i64 :T_int64_t  );
       2:( u64 :T_uint64_t );
       3:( f64 :double   );
     end;

type P_VkPipelineExecutableStatisticKHR = ^VkPipelineExecutableStatisticKHR;
     VkPipelineExecutableStatisticKHR = record
       sType :VkStructureType;
       pNext :P_void;
       name :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       description :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       format :VkPipelineExecutableStatisticFormatKHR;
       value :VkPipelineExecutableStatisticValueKHR;
     end;

type P_VkPipelineExecutableInternalRepresentationKHR = ^VkPipelineExecutableInternalRepresentationKHR;
     VkPipelineExecutableInternalRepresentationKHR = record
       sType :VkStructureType;
       pNext :P_void;
       name :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       description :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       isText :VkBool32;
       dataSize :T_size_t;
       pData :P_void;
     end;

type PFN_vkGetPipelineExecutablePropertiesKHR = function( device_:VkDevice; const pPipelineInfo_:P_VkPipelineInfoKHR; pExecutableCount_:P_uint32_t; pProperties_:P_VkPipelineExecutablePropertiesKHR ) :VkResult;
type PFN_vkGetPipelineExecutableStatisticsKHR = function( device_:VkDevice; const pExecutableInfo_:P_VkPipelineExecutableInfoKHR; pStatisticCount_:P_uint32_t; pStatistics_:P_VkPipelineExecutableStatisticKHR ) :VkResult;
type PFN_vkGetPipelineExecutableInternalRepresentationsKHR = function( device_:VkDevice; const pExecutableInfo_:P_VkPipelineExecutableInfoKHR; pInternalRepresentationCount_:P_uint32_t; pInternalRepresentations_:P_VkPipelineExecutableInternalRepresentationKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPipelineExecutablePropertiesKHR(
    device_:VkDevice;
    pPipelineInfo_:P_VkPipelineInfoKHR;
    pExecutableCount_:P_uint32_t;
    pProperties_:P_VkPipelineExecutablePropertiesKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPipelineExecutableStatisticsKHR(
    device_:VkDevice;
    pExecutableInfo_:P_VkPipelineExecutableInfoKHR;
    pStatisticCount_:P_uint32_t;
    pStatistics_:P_VkPipelineExecutableStatisticKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPipelineExecutableInternalRepresentationsKHR(
    device_:VkDevice;
    pExecutableInfo_:P_VkPipelineExecutableInfoKHR;
    pInternalRepresentationCount_:P_uint32_t;
    pInternalRepresentations_:P_VkPipelineExecutableInternalRepresentationKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_pipeline_library = 1;
const VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION = 1;
const VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME = 'VK_KHR_pipeline_library';
type P_VkPipelineLibraryCreateInfoKHR = ^VkPipelineLibraryCreateInfoKHR;
     VkPipelineLibraryCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       libraryCount :T_uint32_t;
       pLibraries :P_VkPipeline;
     end;



const VK_KHR_shader_non_semantic_info = 1;
const VK_KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION = 1;
const VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME = 'VK_KHR_shader_non_semantic_info';


const VK_KHR_synchronization2 = 1;
type P_VkFlags64 = ^VkFlags64;
     VkFlags64 = T_uint64_t;
const VK_KHR_SYNCHRONIZATION_2_SPEC_VERSION = 1;
const VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME = 'VK_KHR_synchronization2';
type P_VkPipelineStageFlags2KHR = ^VkPipelineStageFlags2KHR;
     VkPipelineStageFlags2KHR = VkFlags64;

// Flag bits for VkPipelineStageFlags2KHR
const VK_PIPELINE_STAGE_2_NONE_KHR                                 :VkPipelineStageFlags2KHR = 0;
const VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR                      :VkPipelineStageFlags2KHR = $00000001;
const VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR                    :VkPipelineStageFlags2KHR = $00000002;
const VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR                     :VkPipelineStageFlags2KHR = $00000004;
const VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR                    :VkPipelineStageFlags2KHR = $00000008;
const VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR      :VkPipelineStageFlags2KHR = $00000010;
const VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR   :VkPipelineStageFlags2KHR = $00000020;
const VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR                  :VkPipelineStageFlags2KHR = $00000040;
const VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR                  :VkPipelineStageFlags2KHR = $00000080;
const VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR             :VkPipelineStageFlags2KHR = $00000100;
const VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR              :VkPipelineStageFlags2KHR = $00000200;
const VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR          :VkPipelineStageFlags2KHR = $00000400;
const VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR                   :VkPipelineStageFlags2KHR = $00000800;
const VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR                     :VkPipelineStageFlags2KHR = $00001000;
const VK_PIPELINE_STAGE_2_TRANSFER_BIT_KHR                         :VkPipelineStageFlags2KHR = $00001000;
const VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR                   :VkPipelineStageFlags2KHR = $00002000;
const VK_PIPELINE_STAGE_2_HOST_BIT_KHR                             :VkPipelineStageFlags2KHR = $00004000;
const VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR                     :VkPipelineStageFlags2KHR = $00008000;
const VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR                     :VkPipelineStageFlags2KHR = $00010000;
const VK_PIPELINE_STAGE_2_COPY_BIT_KHR                             :VkPipelineStageFlags2KHR = UInt64( $100000000 ); {ULL}
const VK_PIPELINE_STAGE_2_RESOLVE_BIT_KHR                          :VkPipelineStageFlags2KHR = UInt64( $200000000 ); {ULL}
const VK_PIPELINE_STAGE_2_BLIT_BIT_KHR                             :VkPipelineStageFlags2KHR = UInt64( $400000000 ); {ULL}
const VK_PIPELINE_STAGE_2_CLEAR_BIT_KHR                            :VkPipelineStageFlags2KHR = UInt64( $800000000 ); {ULL}
const VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR                      :VkPipelineStageFlags2KHR = UInt64( $1000000000 ); {ULL}
const VK_PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR           :VkPipelineStageFlags2KHR = UInt64( $2000000000 ); {ULL}
const VK_PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR        :VkPipelineStageFlags2KHR = UInt64( $4000000000 ); {ULL}
const VK_PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT               :VkPipelineStageFlags2KHR = $01000000;
const VK_PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT            :VkPipelineStageFlags2KHR = $00040000;
const VK_PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV                :VkPipelineStageFlags2KHR = $00020000;
const VK_PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR :VkPipelineStageFlags2KHR = $00400000;
const VK_PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV                :VkPipelineStageFlags2KHR = $00400000;
const VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR     :VkPipelineStageFlags2KHR = $02000000;
const VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR               :VkPipelineStageFlags2KHR = $00200000;
const VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV                :VkPipelineStageFlags2KHR = $00200000;
const VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV      :VkPipelineStageFlags2KHR = $02000000;
const VK_PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT         :VkPipelineStageFlags2KHR = $00800000;
const VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_NV                       :VkPipelineStageFlags2KHR = $00080000;
const VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_NV                       :VkPipelineStageFlags2KHR = $00100000;

type P_VkAccessFlags2KHR = ^VkAccessFlags2KHR;
     VkAccessFlags2KHR = VkFlags64;

// Flag bits for VkAccessFlags2KHR
const VK_ACCESS_2_NONE_KHR                                      :VkAccessFlags2KHR = 0;
const VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR                 :VkAccessFlags2KHR = $00000001;
const VK_ACCESS_2_INDEX_READ_BIT_KHR                            :VkAccessFlags2KHR = $00000002;
const VK_ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR                 :VkAccessFlags2KHR = $00000004;
const VK_ACCESS_2_UNIFORM_READ_BIT_KHR                          :VkAccessFlags2KHR = $00000008;
const VK_ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR                 :VkAccessFlags2KHR = $00000010;
const VK_ACCESS_2_SHADER_READ_BIT_KHR                           :VkAccessFlags2KHR = $00000020;
const VK_ACCESS_2_SHADER_WRITE_BIT_KHR                          :VkAccessFlags2KHR = $00000040;
const VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR                 :VkAccessFlags2KHR = $00000080;
const VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR                :VkAccessFlags2KHR = $00000100;
const VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR         :VkAccessFlags2KHR = $00000200;
const VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR        :VkAccessFlags2KHR = $00000400;
const VK_ACCESS_2_TRANSFER_READ_BIT_KHR                         :VkAccessFlags2KHR = $00000800;
const VK_ACCESS_2_TRANSFER_WRITE_BIT_KHR                        :VkAccessFlags2KHR = $00001000;
const VK_ACCESS_2_HOST_READ_BIT_KHR                             :VkAccessFlags2KHR = $00002000;
const VK_ACCESS_2_HOST_WRITE_BIT_KHR                            :VkAccessFlags2KHR = $00004000;
const VK_ACCESS_2_MEMORY_READ_BIT_KHR                           :VkAccessFlags2KHR = $00008000;
const VK_ACCESS_2_MEMORY_WRITE_BIT_KHR                          :VkAccessFlags2KHR = $00010000;
const VK_ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR                   :VkAccessFlags2KHR = UInt64( $100000000 ); {ULL}
const VK_ACCESS_2_SHADER_STORAGE_READ_BIT_KHR                   :VkAccessFlags2KHR = UInt64( $200000000 ); {ULL}
const VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR                  :VkAccessFlags2KHR = UInt64( $400000000 ); {ULL}
const VK_ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT              :VkAccessFlags2KHR = $02000000;
const VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT       :VkAccessFlags2KHR = $04000000;
const VK_ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT      :VkAccessFlags2KHR = $08000000;
const VK_ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT            :VkAccessFlags2KHR = $00100000;
const VK_ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV                :VkAccessFlags2KHR = $00020000;
const VK_ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV               :VkAccessFlags2KHR = $00040000;
const VK_ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR :VkAccessFlags2KHR = $00800000;
const VK_ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV                :VkAccessFlags2KHR = $00800000;
const VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR           :VkAccessFlags2KHR = $00200000;
const VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR          :VkAccessFlags2KHR = $00400000;
const VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV            :VkAccessFlags2KHR = $00200000;
const VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV           :VkAccessFlags2KHR = $00400000;
const VK_ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT             :VkAccessFlags2KHR = $01000000;
const VK_ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT     :VkAccessFlags2KHR = $00080000;


type P_VkSubmitFlagBitsKHR = ^VkSubmitFlagBitsKHR;
     VkSubmitFlagBitsKHR = (
       VK_SUBMIT_PROTECTED_BIT_KHR = $00000001,
       VK_SUBMIT_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkSubmitFlagsKHR = ^VkSubmitFlagsKHR;
     VkSubmitFlagsKHR = VkFlags;
type P_VkMemoryBarrier2KHR = ^VkMemoryBarrier2KHR;
     VkMemoryBarrier2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcStageMask :VkPipelineStageFlags2KHR;
       srcAccessMask :VkAccessFlags2KHR;
       dstStageMask :VkPipelineStageFlags2KHR;
       dstAccessMask :VkAccessFlags2KHR;
     end;

type P_VkBufferMemoryBarrier2KHR = ^VkBufferMemoryBarrier2KHR;
     VkBufferMemoryBarrier2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcStageMask :VkPipelineStageFlags2KHR;
       srcAccessMask :VkAccessFlags2KHR;
       dstStageMask :VkPipelineStageFlags2KHR;
       dstAccessMask :VkAccessFlags2KHR;
       srcQueueFamilyIndex :T_uint32_t;
       dstQueueFamilyIndex :T_uint32_t;
       buffer :VkBuffer;
       offset :VkDeviceSize;
       size :VkDeviceSize;
     end;

type P_VkImageMemoryBarrier2KHR = ^VkImageMemoryBarrier2KHR;
     VkImageMemoryBarrier2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcStageMask :VkPipelineStageFlags2KHR;
       srcAccessMask :VkAccessFlags2KHR;
       dstStageMask :VkPipelineStageFlags2KHR;
       dstAccessMask :VkAccessFlags2KHR;
       oldLayout :VkImageLayout;
       newLayout :VkImageLayout;
       srcQueueFamilyIndex :T_uint32_t;
       dstQueueFamilyIndex :T_uint32_t;
       image :VkImage;
       subresourceRange :VkImageSubresourceRange;
     end;

type P_VkDependencyInfoKHR = ^VkDependencyInfoKHR;
     VkDependencyInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       dependencyFlags :VkDependencyFlags;
       memoryBarrierCount :T_uint32_t;
       pMemoryBarriers :P_VkMemoryBarrier2KHR;
       bufferMemoryBarrierCount :T_uint32_t;
       pBufferMemoryBarriers :P_VkBufferMemoryBarrier2KHR;
       imageMemoryBarrierCount :T_uint32_t;
       pImageMemoryBarriers :P_VkImageMemoryBarrier2KHR;
     end;

type P_VkSemaphoreSubmitInfoKHR = ^VkSemaphoreSubmitInfoKHR;
     VkSemaphoreSubmitInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       semaphore :VkSemaphore;
       value :T_uint64_t;
       stageMask :VkPipelineStageFlags2KHR;
       deviceIndex :T_uint32_t;
     end;

type P_VkCommandBufferSubmitInfoKHR = ^VkCommandBufferSubmitInfoKHR;
     VkCommandBufferSubmitInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       commandBuffer :VkCommandBuffer;
       deviceMask :T_uint32_t;
     end;

type P_VkSubmitInfo2KHR = ^VkSubmitInfo2KHR;
     VkSubmitInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkSubmitFlagsKHR;
       waitSemaphoreInfoCount :T_uint32_t;
       pWaitSemaphoreInfos :P_VkSemaphoreSubmitInfoKHR;
       commandBufferInfoCount :T_uint32_t;
       pCommandBufferInfos :P_VkCommandBufferSubmitInfoKHR;
       signalSemaphoreInfoCount :T_uint32_t;
       pSignalSemaphoreInfos :P_VkSemaphoreSubmitInfoKHR;
     end;

type P_VkPhysicalDeviceSynchronization2FeaturesKHR = ^VkPhysicalDeviceSynchronization2FeaturesKHR;
     VkPhysicalDeviceSynchronization2FeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       synchronization2 :VkBool32;
     end;

type P_VkQueueFamilyCheckpointProperties2NV = ^VkQueueFamilyCheckpointProperties2NV;
     VkQueueFamilyCheckpointProperties2NV = record
       sType :VkStructureType;
       pNext :P_void;
       checkpointExecutionStageMask :VkPipelineStageFlags2KHR;
     end;

type P_VkCheckpointData2NV = ^VkCheckpointData2NV;
     VkCheckpointData2NV = record
       sType :VkStructureType;
       pNext :P_void;
       stage :VkPipelineStageFlags2KHR;
       pCheckpointMarker :P_void;
     end;

type PFN_vkCmdSetEvent2KHR = procedure( commandBuffer_:VkCommandBuffer; event_:VkEvent; const pDependencyInfo_:P_VkDependencyInfoKHR );
type PFN_vkCmdResetEvent2KHR = procedure( commandBuffer_:VkCommandBuffer; event_:VkEvent; stageMask_:VkPipelineStageFlags2KHR );
type PFN_vkCmdWaitEvents2KHR = procedure( commandBuffer_:VkCommandBuffer; eventCount_:T_uint32_t; const pEvents_:P_VkEvent; const pDependencyInfos_:P_VkDependencyInfoKHR );
type PFN_vkCmdPipelineBarrier2KHR = procedure( commandBuffer_:VkCommandBuffer; const pDependencyInfo_:P_VkDependencyInfoKHR );
type PFN_vkCmdWriteTimestamp2KHR = procedure( commandBuffer_:VkCommandBuffer; stage_:VkPipelineStageFlags2KHR; queryPool_:VkQueryPool; query_:T_uint32_t );
type PFN_vkQueueSubmit2KHR = function( queue_:VkQueue; submitCount_:T_uint32_t; const pSubmits_:P_VkSubmitInfo2KHR; fence_:VkFence ) :VkResult;
type PFN_vkCmdWriteBufferMarker2AMD = procedure( commandBuffer_:VkCommandBuffer; stage_:VkPipelineStageFlags2KHR; dstBuffer_:VkBuffer; dstOffset_:VkDeviceSize; marker_:T_uint32_t );
type PFN_vkGetQueueCheckpointData2NV = procedure( queue_:VkQueue; pCheckpointDataCount_:P_uint32_t; pCheckpointData_:P_VkCheckpointData2NV );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetEvent2KHR(
    commandBuffer_:VkCommandBuffer;
    event_:VkEvent;
    pDependencyInfo_:P_VkDependencyInfoKHR ); stdcall; external DLLNAME;

procedure vkCmdResetEvent2KHR(
    commandBuffer_:VkCommandBuffer;
    event_:VkEvent;
    stageMask_:VkPipelineStageFlags2KHR ); stdcall; external DLLNAME;

procedure vkCmdWaitEvents2KHR(
    commandBuffer_:VkCommandBuffer;
    eventCount_:T_uint32_t;
    pEvents_:P_VkEvent;
    pDependencyInfos_:P_VkDependencyInfoKHR ); stdcall; external DLLNAME;

procedure vkCmdPipelineBarrier2KHR(
    commandBuffer_:VkCommandBuffer;
    pDependencyInfo_:P_VkDependencyInfoKHR ); stdcall; external DLLNAME;

procedure vkCmdWriteTimestamp2KHR(
    commandBuffer_:VkCommandBuffer;
    stage_:VkPipelineStageFlags2KHR;
    queryPool_:VkQueryPool;
    query_:T_uint32_t ); stdcall; external DLLNAME;

function vkQueueSubmit2KHR(
    queue_:VkQueue;
    submitCount_:T_uint32_t;
    pSubmits_:P_VkSubmitInfo2KHR;
    fence_:VkFence ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdWriteBufferMarker2AMD(
    commandBuffer_:VkCommandBuffer;
    stage_:VkPipelineStageFlags2KHR;
    dstBuffer_:VkBuffer;
    dstOffset_:VkDeviceSize;
    marker_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkGetQueueCheckpointData2NV(
    queue_:VkQueue;
    pCheckpointDataCount_:P_uint32_t;
    pCheckpointData_:P_VkCheckpointData2NV ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_zero_initialize_workgroup_memory = 1;
const VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION = 1;
const VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME = 'VK_KHR_zero_initialize_workgroup_memory';
type P_VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR = ^VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR;
     VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       shaderZeroInitializeWorkgroupMemory :VkBool32;
     end;



const VK_KHR_workgroup_memory_explicit_layout = 1;
const VK_KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION = 1;
const VK_KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME = 'VK_KHR_workgroup_memory_explicit_layout';
type P_VkPhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = ^VkPhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR;
     VkPhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       workgroupMemoryExplicitLayout :VkBool32;
       workgroupMemoryExplicitLayoutScalarBlockLayout :VkBool32;
       workgroupMemoryExplicitLayout8BitAccess :VkBool32;
       workgroupMemoryExplicitLayout16BitAccess :VkBool32;
     end;



const VK_KHR_copy_commands2 = 1;
const VK_KHR_COPY_COMMANDS_2_SPEC_VERSION = 1;
const VK_KHR_COPY_COMMANDS_2_EXTENSION_NAME = 'VK_KHR_copy_commands2';
type P_VkBufferCopy2KHR = ^VkBufferCopy2KHR;
     VkBufferCopy2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcOffset :VkDeviceSize;
       dstOffset :VkDeviceSize;
       size :VkDeviceSize;
     end;

type P_VkCopyBufferInfo2KHR = ^VkCopyBufferInfo2KHR;
     VkCopyBufferInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcBuffer :VkBuffer;
       dstBuffer :VkBuffer;
       regionCount :T_uint32_t;
       pRegions :P_VkBufferCopy2KHR;
     end;

type P_VkImageCopy2KHR = ^VkImageCopy2KHR;
     VkImageCopy2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcSubresource :VkImageSubresourceLayers;
       srcOffset :VkOffset3D;
       dstSubresource :VkImageSubresourceLayers;
       dstOffset :VkOffset3D;
       extent :VkExtent3D;
     end;

type P_VkCopyImageInfo2KHR = ^VkCopyImageInfo2KHR;
     VkCopyImageInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcImage :VkImage;
       srcImageLayout :VkImageLayout;
       dstImage :VkImage;
       dstImageLayout :VkImageLayout;
       regionCount :T_uint32_t;
       pRegions :P_VkImageCopy2KHR;
     end;

type P_VkBufferImageCopy2KHR = ^VkBufferImageCopy2KHR;
     VkBufferImageCopy2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       bufferOffset :VkDeviceSize;
       bufferRowLength :T_uint32_t;
       bufferImageHeight :T_uint32_t;
       imageSubresource :VkImageSubresourceLayers;
       imageOffset :VkOffset3D;
       imageExtent :VkExtent3D;
     end;

type P_VkCopyBufferToImageInfo2KHR = ^VkCopyBufferToImageInfo2KHR;
     VkCopyBufferToImageInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcBuffer :VkBuffer;
       dstImage :VkImage;
       dstImageLayout :VkImageLayout;
       regionCount :T_uint32_t;
       pRegions :P_VkBufferImageCopy2KHR;
     end;

type P_VkCopyImageToBufferInfo2KHR = ^VkCopyImageToBufferInfo2KHR;
     VkCopyImageToBufferInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcImage :VkImage;
       srcImageLayout :VkImageLayout;
       dstBuffer :VkBuffer;
       regionCount :T_uint32_t;
       pRegions :P_VkBufferImageCopy2KHR;
     end;

type P_VkImageBlit2KHR = ^VkImageBlit2KHR;
     VkImageBlit2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcSubresource :VkImageSubresourceLayers;
       srcOffsets :array [ 0..2-1 ] of VkOffset3D;
       dstSubresource :VkImageSubresourceLayers;
       dstOffsets :array [ 0..2-1 ] of VkOffset3D;
     end;

type P_VkBlitImageInfo2KHR = ^VkBlitImageInfo2KHR;
     VkBlitImageInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcImage :VkImage;
       srcImageLayout :VkImageLayout;
       dstImage :VkImage;
       dstImageLayout :VkImageLayout;
       regionCount :T_uint32_t;
       pRegions :P_VkImageBlit2KHR;
       filter :VkFilter;
     end;

type P_VkImageResolve2KHR = ^VkImageResolve2KHR;
     VkImageResolve2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcSubresource :VkImageSubresourceLayers;
       srcOffset :VkOffset3D;
       dstSubresource :VkImageSubresourceLayers;
       dstOffset :VkOffset3D;
       extent :VkExtent3D;
     end;

type P_VkResolveImageInfo2KHR = ^VkResolveImageInfo2KHR;
     VkResolveImageInfo2KHR = record
       sType :VkStructureType;
       pNext :P_void;
       srcImage :VkImage;
       srcImageLayout :VkImageLayout;
       dstImage :VkImage;
       dstImageLayout :VkImageLayout;
       regionCount :T_uint32_t;
       pRegions :P_VkImageResolve2KHR;
     end;

type PFN_vkCmdCopyBuffer2KHR = procedure( commandBuffer_:VkCommandBuffer; const pCopyBufferInfo_:P_VkCopyBufferInfo2KHR );
type PFN_vkCmdCopyImage2KHR = procedure( commandBuffer_:VkCommandBuffer; const pCopyImageInfo_:P_VkCopyImageInfo2KHR );
type PFN_vkCmdCopyBufferToImage2KHR = procedure( commandBuffer_:VkCommandBuffer; const pCopyBufferToImageInfo_:P_VkCopyBufferToImageInfo2KHR );
type PFN_vkCmdCopyImageToBuffer2KHR = procedure( commandBuffer_:VkCommandBuffer; const pCopyImageToBufferInfo_:P_VkCopyImageToBufferInfo2KHR );
type PFN_vkCmdBlitImage2KHR = procedure( commandBuffer_:VkCommandBuffer; const pBlitImageInfo_:P_VkBlitImageInfo2KHR );
type PFN_vkCmdResolveImage2KHR = procedure( commandBuffer_:VkCommandBuffer; const pResolveImageInfo_:P_VkResolveImageInfo2KHR );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdCopyBuffer2KHR(
    commandBuffer_:VkCommandBuffer;
    pCopyBufferInfo_:P_VkCopyBufferInfo2KHR ); stdcall; external DLLNAME;

procedure vkCmdCopyImage2KHR(
    commandBuffer_:VkCommandBuffer;
    pCopyImageInfo_:P_VkCopyImageInfo2KHR ); stdcall; external DLLNAME;

procedure vkCmdCopyBufferToImage2KHR(
    commandBuffer_:VkCommandBuffer;
    pCopyBufferToImageInfo_:P_VkCopyBufferToImageInfo2KHR ); stdcall; external DLLNAME;

procedure vkCmdCopyImageToBuffer2KHR(
    commandBuffer_:VkCommandBuffer;
    pCopyImageToBufferInfo_:P_VkCopyImageToBufferInfo2KHR ); stdcall; external DLLNAME;

procedure vkCmdBlitImage2KHR(
    commandBuffer_:VkCommandBuffer;
    pBlitImageInfo_:P_VkBlitImageInfo2KHR ); stdcall; external DLLNAME;

procedure vkCmdResolveImage2KHR(
    commandBuffer_:VkCommandBuffer;
    pResolveImageInfo_:P_VkResolveImageInfo2KHR ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_debug_report = 1;
type P_VkDebugReportCallbackEXT = ^VkDebugReportCallbackEXT;
     VkDebugReportCallbackEXT = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_EXT_DEBUG_REPORT_SPEC_VERSION  = 9;
const VK_EXT_DEBUG_REPORT_EXTENSION_NAME = 'VK_EXT_debug_report';

type P_VkDebugReportObjectTypeEXT = ^VkDebugReportObjectTypeEXT;
     VkDebugReportObjectTypeEXT = (
       VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = 0,
       VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = 1,
       VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = 2,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = 3,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = 4,
       VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = 5,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = 6,
       VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = 7,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = 8,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = 9,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = 10,
       VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = 11,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = 12,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = 13,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = 14,
       VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = 15,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = 16,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = 17,
       VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = 18,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = 19,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = 20,
       VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = 21,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = 22,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = 23,
       VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = 24,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = 25,
       VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = 26,
       VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = 27,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT = 28,
       VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT = 29,
       VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT = 30,
       VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT = 33,
       VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT = 1000156000,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT = 1000085000,
       VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT = 1000150000,
       VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT = 1000165000,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT,
       VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT,
       VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT,
       VK_DEBUG_REPORT_OBJECT_TYPE_MAX_ENUM_EXT = $7FFFFFFF
     );

type P_VkDebugReportFlagBitsEXT = ^VkDebugReportFlagBitsEXT;
     VkDebugReportFlagBitsEXT = (
       VK_DEBUG_REPORT_INFORMATION_BIT_EXT = $00000001,
       VK_DEBUG_REPORT_WARNING_BIT_EXT = $00000002,
       VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = $00000004,
       VK_DEBUG_REPORT_ERROR_BIT_EXT = $00000008,
       VK_DEBUG_REPORT_DEBUG_BIT_EXT = $00000010,
       VK_DEBUG_REPORT_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkDebugReportFlagsEXT = ^VkDebugReportFlagsEXT;
     VkDebugReportFlagsEXT = VkFlags;
type PFN_vkDebugReportCallbackEXT = function(
    flags_:VkDebugReportFlagsEXT;
    objectType_:VkDebugReportObjectTypeEXT;
    object_:T_uint64_t;
    location_:T_size_t;
    messageCode_:T_int32_t;
    pLayerPrefix_:P_char;
    pMessage_:P_char;
    pUserData_:P_void ) :VkBool32;

type P_VkDebugReportCallbackCreateInfoEXT = ^VkDebugReportCallbackCreateInfoEXT;
     VkDebugReportCallbackCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDebugReportFlagsEXT;
       pfnCallback :PFN_vkDebugReportCallbackEXT;
       pUserData :P_void;
     end;

type PFN_vkCreateDebugReportCallbackEXT = function( instance_:VkInstance; const pCreateInfo_:P_VkDebugReportCallbackCreateInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pCallback_:P_VkDebugReportCallbackEXT ) :VkResult;
type PFN_vkDestroyDebugReportCallbackEXT = procedure( instance_:VkInstance; callback_:VkDebugReportCallbackEXT; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkDebugReportMessageEXT = procedure( instance_:VkInstance; flags_:VkDebugReportFlagsEXT; objectType_:VkDebugReportObjectTypeEXT; object_:T_uint64_t; location_:T_size_t; messageCode_:T_int32_t; const pLayerPrefix_:P_char; const pMessage_:P_char );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateDebugReportCallbackEXT(
    instance_:VkInstance;
    pCreateInfo_:P_VkDebugReportCallbackCreateInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pCallback_:P_VkDebugReportCallbackEXT ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDebugReportCallbackEXT(
    instance_:VkInstance;
    callback_:VkDebugReportCallbackEXT;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkDebugReportMessageEXT(
    instance_:VkInstance;
    flags_:VkDebugReportFlagsEXT;
    objectType_:VkDebugReportObjectTypeEXT;
    object_:T_uint64_t;
    location_:T_size_t;
    messageCode_:T_int32_t;
    pLayerPrefix_:P_char;
    pMessage_:P_char ); stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_glsl_shader = 1;
const VK_NV_GLSL_SHADER_SPEC_VERSION    = 1;
const VK_NV_GLSL_SHADER_EXTENSION_NAME = 'VK_NV_glsl_shader';


const VK_EXT_depth_range_unrestricted = 1;
const VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = 1;
const VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = 'VK_EXT_depth_range_unrestricted';


const VK_IMG_filter_cubic = 1;
const VK_IMG_FILTER_CUBIC_SPEC_VERSION  = 1;
const VK_IMG_FILTER_CUBIC_EXTENSION_NAME = 'VK_IMG_filter_cubic';


const VK_AMD_rasterization_order = 1;
const VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1;
const VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME = 'VK_AMD_rasterization_order';

type P_VkRasterizationOrderAMD = ^VkRasterizationOrderAMD;
     VkRasterizationOrderAMD = (
       VK_RASTERIZATION_ORDER_STRICT_AMD = 0,
       VK_RASTERIZATION_ORDER_RELAXED_AMD = 1,
       VK_RASTERIZATION_ORDER_MAX_ENUM_AMD = $7FFFFFFF
     );
type P_VkPipelineRasterizationStateRasterizationOrderAMD = ^VkPipelineRasterizationStateRasterizationOrderAMD;
     VkPipelineRasterizationStateRasterizationOrderAMD = record
       sType :VkStructureType;
       pNext :P_void;
       rasterizationOrder :VkRasterizationOrderAMD;
     end;



const VK_AMD_shader_trinary_minmax = 1;
const VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION = 1;
const VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME = 'VK_AMD_shader_trinary_minmax';


const VK_AMD_shader_explicit_vertex_parameter = 1;
const VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION = 1;
const VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME = 'VK_AMD_shader_explicit_vertex_parameter';


const VK_EXT_debug_marker = 1;
const VK_EXT_DEBUG_MARKER_SPEC_VERSION  = 4;
const VK_EXT_DEBUG_MARKER_EXTENSION_NAME = 'VK_EXT_debug_marker';
type P_VkDebugMarkerObjectNameInfoEXT = ^VkDebugMarkerObjectNameInfoEXT;
     VkDebugMarkerObjectNameInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       objectType :VkDebugReportObjectTypeEXT;
       object_ :T_uint64_t;
       pObjectName :P_char;
     end;

type P_VkDebugMarkerObjectTagInfoEXT = ^VkDebugMarkerObjectTagInfoEXT;
     VkDebugMarkerObjectTagInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       objectType :VkDebugReportObjectTypeEXT;
       object_ :T_uint64_t;
       tagName :T_uint64_t;
       tagSize :T_size_t;
       pTag :P_void;
     end;

type P_VkDebugMarkerMarkerInfoEXT = ^VkDebugMarkerMarkerInfoEXT;
     VkDebugMarkerMarkerInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       pMarkerName :P_char;
       color :array [ 0..4-1 ] of T_float;
     end;

type PFN_vkDebugMarkerSetObjectTagEXT = function( device_:VkDevice; const pTagInfo_:P_VkDebugMarkerObjectTagInfoEXT ) :VkResult;
type PFN_vkDebugMarkerSetObjectNameEXT = function( device_:VkDevice; const pNameInfo_:P_VkDebugMarkerObjectNameInfoEXT ) :VkResult;
type PFN_vkCmdDebugMarkerBeginEXT = procedure( commandBuffer_:VkCommandBuffer; const pMarkerInfo_:P_VkDebugMarkerMarkerInfoEXT );
type PFN_vkCmdDebugMarkerEndEXT = procedure( commandBuffer_:VkCommandBuffer );
type PFN_vkCmdDebugMarkerInsertEXT = procedure( commandBuffer_:VkCommandBuffer; const pMarkerInfo_:P_VkDebugMarkerMarkerInfoEXT );

{$IFNDEF VK_NO_PROTOTYPES }
function vkDebugMarkerSetObjectTagEXT(
    device_:VkDevice;
    pTagInfo_:P_VkDebugMarkerObjectTagInfoEXT ) :VkResult; stdcall; external DLLNAME;

function vkDebugMarkerSetObjectNameEXT(
    device_:VkDevice;
    pNameInfo_:P_VkDebugMarkerObjectNameInfoEXT ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdDebugMarkerBeginEXT(
    commandBuffer_:VkCommandBuffer;
    pMarkerInfo_:P_VkDebugMarkerMarkerInfoEXT ); stdcall; external DLLNAME;

procedure vkCmdDebugMarkerEndEXT(
    commandBuffer_:VkCommandBuffer ); stdcall; external DLLNAME;

procedure vkCmdDebugMarkerInsertEXT(
    commandBuffer_:VkCommandBuffer;
    pMarkerInfo_:P_VkDebugMarkerMarkerInfoEXT ); stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_gcn_shader = 1;
const VK_AMD_GCN_SHADER_SPEC_VERSION    = 1;
const VK_AMD_GCN_SHADER_EXTENSION_NAME = 'VK_AMD_gcn_shader';


const VK_NV_dedicated_allocation = 1;
const VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1;
const VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME = 'VK_NV_dedicated_allocation';
type P_VkDedicatedAllocationImageCreateInfoNV = ^VkDedicatedAllocationImageCreateInfoNV;
     VkDedicatedAllocationImageCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       dedicatedAllocation :VkBool32;
     end;

type P_VkDedicatedAllocationBufferCreateInfoNV = ^VkDedicatedAllocationBufferCreateInfoNV;
     VkDedicatedAllocationBufferCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       dedicatedAllocation :VkBool32;
     end;

type P_VkDedicatedAllocationMemoryAllocateInfoNV = ^VkDedicatedAllocationMemoryAllocateInfoNV;
     VkDedicatedAllocationMemoryAllocateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       image :VkImage;
       buffer :VkBuffer;
     end;



const VK_EXT_transform_feedback = 1;
const VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1;
const VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME = 'VK_EXT_transform_feedback';
type P_VkPipelineRasterizationStateStreamCreateFlagsEXT = ^VkPipelineRasterizationStateStreamCreateFlagsEXT;
     VkPipelineRasterizationStateStreamCreateFlagsEXT = VkFlags;
type P_VkPhysicalDeviceTransformFeedbackFeaturesEXT = ^VkPhysicalDeviceTransformFeedbackFeaturesEXT;
     VkPhysicalDeviceTransformFeedbackFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       transformFeedback :VkBool32;
       geometryStreams :VkBool32;
     end;

type P_VkPhysicalDeviceTransformFeedbackPropertiesEXT = ^VkPhysicalDeviceTransformFeedbackPropertiesEXT;
     VkPhysicalDeviceTransformFeedbackPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxTransformFeedbackStreams :T_uint32_t;
       maxTransformFeedbackBuffers :T_uint32_t;
       maxTransformFeedbackBufferSize :VkDeviceSize;
       maxTransformFeedbackStreamDataSize :T_uint32_t;
       maxTransformFeedbackBufferDataSize :T_uint32_t;
       maxTransformFeedbackBufferDataStride :T_uint32_t;
       transformFeedbackQueries :VkBool32;
       transformFeedbackStreamsLinesTriangles :VkBool32;
       transformFeedbackRasterizationStreamSelect :VkBool32;
       transformFeedbackDraw :VkBool32;
     end;

type P_VkPipelineRasterizationStateStreamCreateInfoEXT = ^VkPipelineRasterizationStateStreamCreateInfoEXT;
     VkPipelineRasterizationStateStreamCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineRasterizationStateStreamCreateFlagsEXT;
       rasterizationStream :T_uint32_t;
     end;

type PFN_vkCmdBindTransformFeedbackBuffersEXT = procedure( commandBuffer_:VkCommandBuffer; firstBinding_:T_uint32_t; bindingCount_:T_uint32_t; const pBuffers_:P_VkBuffer; const pOffsets_:P_VkDeviceSize; const pSizes_:P_VkDeviceSize );
type PFN_vkCmdBeginTransformFeedbackEXT = procedure( commandBuffer_:VkCommandBuffer; firstCounterBuffer_:T_uint32_t; counterBufferCount_:T_uint32_t; const pCounterBuffers_:P_VkBuffer; const pCounterBufferOffsets_:P_VkDeviceSize );
type PFN_vkCmdEndTransformFeedbackEXT = procedure( commandBuffer_:VkCommandBuffer; firstCounterBuffer_:T_uint32_t; counterBufferCount_:T_uint32_t; const pCounterBuffers_:P_VkBuffer; const pCounterBufferOffsets_:P_VkDeviceSize );
type PFN_vkCmdBeginQueryIndexedEXT = procedure( commandBuffer_:VkCommandBuffer; queryPool_:VkQueryPool; query_:T_uint32_t; flags_:VkQueryControlFlags; index_:T_uint32_t );
type PFN_vkCmdEndQueryIndexedEXT = procedure( commandBuffer_:VkCommandBuffer; queryPool_:VkQueryPool; query_:T_uint32_t; index_:T_uint32_t );
type PFN_vkCmdDrawIndirectByteCountEXT = procedure( commandBuffer_:VkCommandBuffer; instanceCount_:T_uint32_t; firstInstance_:T_uint32_t; counterBuffer_:VkBuffer; counterBufferOffset_:VkDeviceSize; counterOffset_:T_uint32_t; vertexStride_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdBindTransformFeedbackBuffersEXT(
    commandBuffer_:VkCommandBuffer;
    firstBinding_:T_uint32_t;
    bindingCount_:T_uint32_t;
    pBuffers_:P_VkBuffer;
    pOffsets_:P_VkDeviceSize;
    pSizes_:P_VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdBeginTransformFeedbackEXT(
    commandBuffer_:VkCommandBuffer;
    firstCounterBuffer_:T_uint32_t;
    counterBufferCount_:T_uint32_t;
    pCounterBuffers_:P_VkBuffer;
    pCounterBufferOffsets_:P_VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdEndTransformFeedbackEXT(
    commandBuffer_:VkCommandBuffer;
    firstCounterBuffer_:T_uint32_t;
    counterBufferCount_:T_uint32_t;
    pCounterBuffers_:P_VkBuffer;
    pCounterBufferOffsets_:P_VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdBeginQueryIndexedEXT(
    commandBuffer_:VkCommandBuffer;
    queryPool_:VkQueryPool;
    query_:T_uint32_t;
    flags_:VkQueryControlFlags;
    index_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdEndQueryIndexedEXT(
    commandBuffer_:VkCommandBuffer;
    queryPool_:VkQueryPool;
    query_:T_uint32_t;
    index_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndirectByteCountEXT(
    commandBuffer_:VkCommandBuffer;
    instanceCount_:T_uint32_t;
    firstInstance_:T_uint32_t;
    counterBuffer_:VkBuffer;
    counterBufferOffset_:VkDeviceSize;
    counterOffset_:T_uint32_t;
    vertexStride_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_NVX_image_view_handle = 1;
const VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 2;
const VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME = 'VK_NVX_image_view_handle';
type P_VkImageViewHandleInfoNVX = ^VkImageViewHandleInfoNVX;
     VkImageViewHandleInfoNVX = record
       sType :VkStructureType;
       pNext :P_void;
       imageView :VkImageView;
       descriptorType :VkDescriptorType;
       sampler :VkSampler;
     end;

type P_VkImageViewAddressPropertiesNVX = ^VkImageViewAddressPropertiesNVX;
     VkImageViewAddressPropertiesNVX = record
       sType :VkStructureType;
       pNext :P_void;
       deviceAddress :VkDeviceAddress;
       size :VkDeviceSize;
     end;

type PFN_vkGetImageViewHandleNVX = function( device_:VkDevice; const pInfo_:P_VkImageViewHandleInfoNVX ) :T_uint32_t;
type PFN_vkGetImageViewAddressNVX = function( device_:VkDevice; imageView_:VkImageView; pProperties_:P_VkImageViewAddressPropertiesNVX ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetImageViewHandleNVX(
    device_:VkDevice;
    pInfo_:P_VkImageViewHandleInfoNVX ) :T_uint32_t; stdcall; external DLLNAME;

function vkGetImageViewAddressNVX(
    device_:VkDevice;
    imageView_:VkImageView;
    pProperties_:P_VkImageViewAddressPropertiesNVX ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_draw_indirect_count = 1;
const VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 2;
const VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = 'VK_AMD_draw_indirect_count';
type PFN_vkCmdDrawIndirectCountAMD = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCmdDrawIndexedIndirectCountAMD = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdDrawIndirectCountAMD(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexedIndirectCountAMD(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_negative_viewport_height = 1;
const VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1;
const VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = 'VK_AMD_negative_viewport_height';


const VK_AMD_gpu_shader_half_float = 1;
const VK_AMD_GPU_SHADER_HALF_FLOAT_SPEC_VERSION = 2;
const VK_AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME = 'VK_AMD_gpu_shader_half_float';


const VK_AMD_shader_ballot = 1;
const VK_AMD_SHADER_BALLOT_SPEC_VERSION = 1;
const VK_AMD_SHADER_BALLOT_EXTENSION_NAME = 'VK_AMD_shader_ballot';


const VK_AMD_texture_gather_bias_lod = 1;
const VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1;
const VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME = 'VK_AMD_texture_gather_bias_lod';
type P_VkTextureLODGatherFormatPropertiesAMD = ^VkTextureLODGatherFormatPropertiesAMD;
     VkTextureLODGatherFormatPropertiesAMD = record
       sType :VkStructureType;
       pNext :P_void;
       supportsTextureGatherLODBiasAMD :VkBool32;
     end;



const VK_AMD_shader_info = 1;
const VK_AMD_SHADER_INFO_SPEC_VERSION   = 1;
const VK_AMD_SHADER_INFO_EXTENSION_NAME = 'VK_AMD_shader_info';

type P_VkShaderInfoTypeAMD = ^VkShaderInfoTypeAMD;
     VkShaderInfoTypeAMD = (
       VK_SHADER_INFO_TYPE_STATISTICS_AMD = 0,
       VK_SHADER_INFO_TYPE_BINARY_AMD = 1,
       VK_SHADER_INFO_TYPE_DISASSEMBLY_AMD = 2,
       VK_SHADER_INFO_TYPE_MAX_ENUM_AMD = $7FFFFFFF
     );
type P_VkShaderResourceUsageAMD = ^VkShaderResourceUsageAMD;
     VkShaderResourceUsageAMD = record
       numUsedVgprs :T_uint32_t;
       numUsedSgprs :T_uint32_t;
       ldsSizePerLocalWorkGroup :T_uint32_t;
       ldsUsageSizeInBytes :T_size_t;
       scratchMemUsageInBytes :T_size_t;
     end;

type P_VkShaderStatisticsInfoAMD = ^VkShaderStatisticsInfoAMD;
     VkShaderStatisticsInfoAMD = record
       shaderStageMask :VkShaderStageFlags;
       resourceUsage :VkShaderResourceUsageAMD;
       numPhysicalVgprs :T_uint32_t;
       numPhysicalSgprs :T_uint32_t;
       numAvailableVgprs :T_uint32_t;
       numAvailableSgprs :T_uint32_t;
       computeWorkGroupSize :array [ 0..3-1 ] of T_uint32_t;
     end;

type PFN_vkGetShaderInfoAMD = function( device_:VkDevice; pipeline_:VkPipeline; shaderStage_:VkShaderStageFlagBits; infoType_:VkShaderInfoTypeAMD; pInfoSize_:P_size_t; pInfo_:P_void ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetShaderInfoAMD(
    device_:VkDevice;
    pipeline_:VkPipeline;
    shaderStage_:VkShaderStageFlagBits;
    infoType_:VkShaderInfoTypeAMD;
    pInfoSize_:P_size_t;
    pInfo_:P_void ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_shader_image_load_store_lod = 1;
const VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_SPEC_VERSION = 1;
const VK_AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME = 'VK_AMD_shader_image_load_store_lod';


const VK_NV_corner_sampled_image = 1;
const VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = 2;
const VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = 'VK_NV_corner_sampled_image';
type P_VkPhysicalDeviceCornerSampledImageFeaturesNV = ^VkPhysicalDeviceCornerSampledImageFeaturesNV;
     VkPhysicalDeviceCornerSampledImageFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       cornerSampledImage :VkBool32;
     end;



const VK_IMG_format_pvrtc = 1;
const VK_IMG_FORMAT_PVRTC_SPEC_VERSION  = 1;
const VK_IMG_FORMAT_PVRTC_EXTENSION_NAME = 'VK_IMG_format_pvrtc';


const VK_NV_external_memory_capabilities = 1;
const VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1;
const VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = 'VK_NV_external_memory_capabilities';

type P_VkExternalMemoryHandleTypeFlagBitsNV = ^VkExternalMemoryHandleTypeFlagBitsNV;
     VkExternalMemoryHandleTypeFlagBitsNV = (
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = $00000001,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = $00000002,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = $00000004,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = $00000008,
       VK_EXTERNAL_MEMORY_HANDLE_TYPE_FLAG_BITS_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkExternalMemoryHandleTypeFlagsNV = ^VkExternalMemoryHandleTypeFlagsNV;
     VkExternalMemoryHandleTypeFlagsNV = VkFlags;

type P_VkExternalMemoryFeatureFlagBitsNV = ^VkExternalMemoryFeatureFlagBitsNV;
     VkExternalMemoryFeatureFlagBitsNV = (
       VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = $00000001,
       VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = $00000002,
       VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = $00000004,
       VK_EXTERNAL_MEMORY_FEATURE_FLAG_BITS_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkExternalMemoryFeatureFlagsNV = ^VkExternalMemoryFeatureFlagsNV;
     VkExternalMemoryFeatureFlagsNV = VkFlags;
type P_VkExternalImageFormatPropertiesNV = ^VkExternalImageFormatPropertiesNV;
     VkExternalImageFormatPropertiesNV = record
       imageFormatProperties :VkImageFormatProperties;
       externalMemoryFeatures :VkExternalMemoryFeatureFlagsNV;
       exportFromImportedHandleTypes :VkExternalMemoryHandleTypeFlagsNV;
       compatibleHandleTypes :VkExternalMemoryHandleTypeFlagsNV;
     end;

type PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV = function( physicalDevice_:VkPhysicalDevice; format_:VkFormat; type_:VkImageType; tiling_:VkImageTiling; usage_:VkImageUsageFlags; flags_:VkImageCreateFlags; externalHandleType_:VkExternalMemoryHandleTypeFlagsNV; pExternalImageFormatProperties_:P_VkExternalImageFormatPropertiesNV ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceExternalImageFormatPropertiesNV(
    physicalDevice_:VkPhysicalDevice;
    format_:VkFormat;
    type_:VkImageType;
    tiling_:VkImageTiling;
    usage_:VkImageUsageFlags;
    flags_:VkImageCreateFlags;
    externalHandleType_:VkExternalMemoryHandleTypeFlagsNV;
    pExternalImageFormatProperties_:P_VkExternalImageFormatPropertiesNV ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_external_memory = 1;
const VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1;
const VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = 'VK_NV_external_memory';
type P_VkExternalMemoryImageCreateInfoNV = ^VkExternalMemoryImageCreateInfoNV;
     VkExternalMemoryImageCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       handleTypes :VkExternalMemoryHandleTypeFlagsNV;
     end;

type P_VkExportMemoryAllocateInfoNV = ^VkExportMemoryAllocateInfoNV;
     VkExportMemoryAllocateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       handleTypes :VkExternalMemoryHandleTypeFlagsNV;
     end;



const VK_EXT_validation_flags = 1;
const VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 2;
const VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME = 'VK_EXT_validation_flags';

type P_VkValidationCheckEXT = ^VkValidationCheckEXT;
     VkValidationCheckEXT = (
       VK_VALIDATION_CHECK_ALL_EXT = 0,
       VK_VALIDATION_CHECK_SHADERS_EXT = 1,
       VK_VALIDATION_CHECK_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkValidationFlagsEXT = ^VkValidationFlagsEXT;
     VkValidationFlagsEXT = record
       sType :VkStructureType;
       pNext :P_void;
       disabledValidationCheckCount :T_uint32_t;
       pDisabledValidationChecks :P_VkValidationCheckEXT;
     end;



const VK_EXT_shader_subgroup_ballot = 1;
const VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION = 1;
const VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME = 'VK_EXT_shader_subgroup_ballot';


const VK_EXT_shader_subgroup_vote = 1;
const VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION = 1;
const VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME = 'VK_EXT_shader_subgroup_vote';


const VK_EXT_texture_compression_astc_hdr = 1;
const VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1;
const VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = 'VK_EXT_texture_compression_astc_hdr';
type P_VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = ^VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT;
     VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       textureCompressionASTC_HDR :VkBool32;
     end;



const VK_EXT_astc_decode_mode = 1;
const VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1;
const VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME = 'VK_EXT_astc_decode_mode';
type P_VkImageViewASTCDecodeModeEXT = ^VkImageViewASTCDecodeModeEXT;
     VkImageViewASTCDecodeModeEXT = record
       sType :VkStructureType;
       pNext :P_void;
       decodeMode :VkFormat;
     end;

type P_VkPhysicalDeviceASTCDecodeFeaturesEXT = ^VkPhysicalDeviceASTCDecodeFeaturesEXT;
     VkPhysicalDeviceASTCDecodeFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       decodeModeSharedExponent :VkBool32;
     end;



const VK_EXT_conditional_rendering = 1;
const VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2;
const VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = 'VK_EXT_conditional_rendering';

type P_VkConditionalRenderingFlagBitsEXT = ^VkConditionalRenderingFlagBitsEXT;
     VkConditionalRenderingFlagBitsEXT = (
       VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = $00000001,
       VK_CONDITIONAL_RENDERING_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkConditionalRenderingFlagsEXT = ^VkConditionalRenderingFlagsEXT;
     VkConditionalRenderingFlagsEXT = VkFlags;
type P_VkConditionalRenderingBeginInfoEXT = ^VkConditionalRenderingBeginInfoEXT;
     VkConditionalRenderingBeginInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       buffer :VkBuffer;
       offset :VkDeviceSize;
       flags :VkConditionalRenderingFlagsEXT;
     end;

type P_VkPhysicalDeviceConditionalRenderingFeaturesEXT = ^VkPhysicalDeviceConditionalRenderingFeaturesEXT;
     VkPhysicalDeviceConditionalRenderingFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       conditionalRendering :VkBool32;
       inheritedConditionalRendering :VkBool32;
     end;

type P_VkCommandBufferInheritanceConditionalRenderingInfoEXT = ^VkCommandBufferInheritanceConditionalRenderingInfoEXT;
     VkCommandBufferInheritanceConditionalRenderingInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       conditionalRenderingEnable :VkBool32;
     end;

type PFN_vkCmdBeginConditionalRenderingEXT = procedure( commandBuffer_:VkCommandBuffer; const pConditionalRenderingBegin_:P_VkConditionalRenderingBeginInfoEXT );
type PFN_vkCmdEndConditionalRenderingEXT = procedure( commandBuffer_:VkCommandBuffer );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdBeginConditionalRenderingEXT(
    commandBuffer_:VkCommandBuffer;
    pConditionalRenderingBegin_:P_VkConditionalRenderingBeginInfoEXT ); stdcall; external DLLNAME;

procedure vkCmdEndConditionalRenderingEXT(
    commandBuffer_:VkCommandBuffer ); stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_clip_space_w_scaling = 1;
const VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1;
const VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = 'VK_NV_clip_space_w_scaling';
type P_VkViewportWScalingNV = ^VkViewportWScalingNV;
     VkViewportWScalingNV = record
       xcoeff :T_float;
       ycoeff :T_float;
     end;

type P_VkPipelineViewportWScalingStateCreateInfoNV = ^VkPipelineViewportWScalingStateCreateInfoNV;
     VkPipelineViewportWScalingStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       viewportWScalingEnable :VkBool32;
       viewportCount :T_uint32_t;
       pViewportWScalings :P_VkViewportWScalingNV;
     end;

type PFN_vkCmdSetViewportWScalingNV = procedure( commandBuffer_:VkCommandBuffer; firstViewport_:T_uint32_t; viewportCount_:T_uint32_t; const pViewportWScalings_:P_VkViewportWScalingNV );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetViewportWScalingNV(
    commandBuffer_:VkCommandBuffer;
    firstViewport_:T_uint32_t;
    viewportCount_:T_uint32_t;
    pViewportWScalings_:P_VkViewportWScalingNV ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_direct_mode_display = 1;
const VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1;
const VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = 'VK_EXT_direct_mode_display';
type PFN_vkReleaseDisplayEXT = function( physicalDevice_:VkPhysicalDevice; display_:VkDisplayKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkReleaseDisplayEXT(
    physicalDevice_:VkPhysicalDevice;
    display_:VkDisplayKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_display_surface_counter = 1;
const VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1;
const VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME = 'VK_EXT_display_surface_counter';

type P_VkSurfaceCounterFlagBitsEXT = ^VkSurfaceCounterFlagBitsEXT;
     VkSurfaceCounterFlagBitsEXT = (
       VK_SURFACE_COUNTER_VBLANK_BIT_EXT = $00000001,
       VK_SURFACE_COUNTER_VBLANK_EXT = VK_SURFACE_COUNTER_VBLANK_BIT_EXT,
       VK_SURFACE_COUNTER_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkSurfaceCounterFlagsEXT = ^VkSurfaceCounterFlagsEXT;
     VkSurfaceCounterFlagsEXT = VkFlags;
type P_VkSurfaceCapabilities2EXT = ^VkSurfaceCapabilities2EXT;
     VkSurfaceCapabilities2EXT = record
       sType :VkStructureType;
       pNext :P_void;
       minImageCount :T_uint32_t;
       maxImageCount :T_uint32_t;
       currentExtent :VkExtent2D;
       minImageExtent :VkExtent2D;
       maxImageExtent :VkExtent2D;
       maxImageArrayLayers :T_uint32_t;
       supportedTransforms :VkSurfaceTransformFlagsKHR;
       currentTransform :VkSurfaceTransformFlagBitsKHR;
       supportedCompositeAlpha :VkCompositeAlphaFlagsKHR;
       supportedUsageFlags :VkImageUsageFlags;
       supportedSurfaceCounters :VkSurfaceCounterFlagsEXT;
     end;

type PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT = function( physicalDevice_:VkPhysicalDevice; surface_:VkSurfaceKHR; pSurfaceCapabilities_:P_VkSurfaceCapabilities2EXT ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceSurfaceCapabilities2EXT(
    physicalDevice_:VkPhysicalDevice;
    surface_:VkSurfaceKHR;
    pSurfaceCapabilities_:P_VkSurfaceCapabilities2EXT ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_display_control = 1;
const VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1;
const VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME = 'VK_EXT_display_control';

type P_VkDisplayPowerStateEXT = ^VkDisplayPowerStateEXT;
     VkDisplayPowerStateEXT = (
       VK_DISPLAY_POWER_STATE_OFF_EXT = 0,
       VK_DISPLAY_POWER_STATE_SUSPEND_EXT = 1,
       VK_DISPLAY_POWER_STATE_ON_EXT = 2,
       VK_DISPLAY_POWER_STATE_MAX_ENUM_EXT = $7FFFFFFF
     );

type P_VkDeviceEventTypeEXT = ^VkDeviceEventTypeEXT;
     VkDeviceEventTypeEXT = (
       VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = 0,
       VK_DEVICE_EVENT_TYPE_MAX_ENUM_EXT = $7FFFFFFF
     );

type P_VkDisplayEventTypeEXT = ^VkDisplayEventTypeEXT;
     VkDisplayEventTypeEXT = (
       VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = 0,
       VK_DISPLAY_EVENT_TYPE_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkDisplayPowerInfoEXT = ^VkDisplayPowerInfoEXT;
     VkDisplayPowerInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       powerState :VkDisplayPowerStateEXT;
     end;

type P_VkDeviceEventInfoEXT = ^VkDeviceEventInfoEXT;
     VkDeviceEventInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       deviceEvent :VkDeviceEventTypeEXT;
     end;

type P_VkDisplayEventInfoEXT = ^VkDisplayEventInfoEXT;
     VkDisplayEventInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       displayEvent :VkDisplayEventTypeEXT;
     end;

type P_VkSwapchainCounterCreateInfoEXT = ^VkSwapchainCounterCreateInfoEXT;
     VkSwapchainCounterCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       surfaceCounters :VkSurfaceCounterFlagsEXT;
     end;

type PFN_vkDisplayPowerControlEXT = function( device_:VkDevice; display_:VkDisplayKHR; const pDisplayPowerInfo_:P_VkDisplayPowerInfoEXT ) :VkResult;
type PFN_vkRegisterDeviceEventEXT = function( device_:VkDevice; const pDeviceEventInfo_:P_VkDeviceEventInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pFence_:P_VkFence ) :VkResult;
type PFN_vkRegisterDisplayEventEXT = function( device_:VkDevice; display_:VkDisplayKHR; const pDisplayEventInfo_:P_VkDisplayEventInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pFence_:P_VkFence ) :VkResult;
type PFN_vkGetSwapchainCounterEXT = function( device_:VkDevice; swapchain_:VkSwapchainKHR; counter_:VkSurfaceCounterFlagBitsEXT; pCounterValue_:P_uint64_t ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkDisplayPowerControlEXT(
    device_:VkDevice;
    display_:VkDisplayKHR;
    pDisplayPowerInfo_:P_VkDisplayPowerInfoEXT ) :VkResult; stdcall; external DLLNAME;

function vkRegisterDeviceEventEXT(
    device_:VkDevice;
    pDeviceEventInfo_:P_VkDeviceEventInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pFence_:P_VkFence ) :VkResult; stdcall; external DLLNAME;

function vkRegisterDisplayEventEXT(
    device_:VkDevice;
    display_:VkDisplayKHR;
    pDisplayEventInfo_:P_VkDisplayEventInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pFence_:P_VkFence ) :VkResult; stdcall; external DLLNAME;

function vkGetSwapchainCounterEXT(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR;
    counter_:VkSurfaceCounterFlagBitsEXT;
    pCounterValue_:P_uint64_t ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_GOOGLE_display_timing = 1;
const VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION = 1;
const VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME = 'VK_GOOGLE_display_timing';
type P_VkRefreshCycleDurationGOOGLE = ^VkRefreshCycleDurationGOOGLE;
     VkRefreshCycleDurationGOOGLE = record
       refreshDuration :T_uint64_t;
     end;

type P_VkPastPresentationTimingGOOGLE = ^VkPastPresentationTimingGOOGLE;
     VkPastPresentationTimingGOOGLE = record
       presentID :T_uint32_t;
       desiredPresentTime :T_uint64_t;
       actualPresentTime :T_uint64_t;
       earliestPresentTime :T_uint64_t;
       presentMargin :T_uint64_t;
     end;

type P_VkPresentTimeGOOGLE = ^VkPresentTimeGOOGLE;
     VkPresentTimeGOOGLE = record
       presentID :T_uint32_t;
       desiredPresentTime :T_uint64_t;
     end;

type P_VkPresentTimesInfoGOOGLE = ^VkPresentTimesInfoGOOGLE;
     VkPresentTimesInfoGOOGLE = record
       sType :VkStructureType;
       pNext :P_void;
       swapchainCount :T_uint32_t;
       pTimes :P_VkPresentTimeGOOGLE;
     end;

type PFN_vkGetRefreshCycleDurationGOOGLE = function( device_:VkDevice; swapchain_:VkSwapchainKHR; pDisplayTimingProperties_:P_VkRefreshCycleDurationGOOGLE ) :VkResult;
type PFN_vkGetPastPresentationTimingGOOGLE = function( device_:VkDevice; swapchain_:VkSwapchainKHR; pPresentationTimingCount_:P_uint32_t; pPresentationTimings_:P_VkPastPresentationTimingGOOGLE ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetRefreshCycleDurationGOOGLE(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR;
    pDisplayTimingProperties_:P_VkRefreshCycleDurationGOOGLE ) :VkResult; stdcall; external DLLNAME;

function vkGetPastPresentationTimingGOOGLE(
    device_:VkDevice;
    swapchain_:VkSwapchainKHR;
    pPresentationTimingCount_:P_uint32_t;
    pPresentationTimings_:P_VkPastPresentationTimingGOOGLE ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_sample_mask_override_coverage = 1;
const VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1;
const VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = 'VK_NV_sample_mask_override_coverage';


const VK_NV_geometry_shader_passthrough = 1;
const VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1;
const VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME = 'VK_NV_geometry_shader_passthrough';


const VK_NV_viewport_array2 = 1;
const VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION = 1;
const VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME = 'VK_NV_viewport_array2';


const VK_NVX_multiview_per_view_attributes = 1;
const VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1;
const VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = 'VK_NVX_multiview_per_view_attributes';
type P_VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = ^VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
     VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = record
       sType :VkStructureType;
       pNext :P_void;
       perViewPositionAllComponents :VkBool32;
     end;



const VK_NV_viewport_swizzle = 1;
const VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1;
const VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME = 'VK_NV_viewport_swizzle';

type P_VkViewportCoordinateSwizzleNV = ^VkViewportCoordinateSwizzleNV;
     VkViewportCoordinateSwizzleNV = (
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = 0,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = 1,
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = 2,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = 3,
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = 4,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = 5,
       VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = 6,
       VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = 7,
       VK_VIEWPORT_COORDINATE_SWIZZLE_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkPipelineViewportSwizzleStateCreateFlagsNV = ^VkPipelineViewportSwizzleStateCreateFlagsNV;
     VkPipelineViewportSwizzleStateCreateFlagsNV = VkFlags;
type P_VkViewportSwizzleNV = ^VkViewportSwizzleNV;
     VkViewportSwizzleNV = record
       x :VkViewportCoordinateSwizzleNV;
       y :VkViewportCoordinateSwizzleNV;
       z :VkViewportCoordinateSwizzleNV;
       w :VkViewportCoordinateSwizzleNV;
     end;

type P_VkPipelineViewportSwizzleStateCreateInfoNV = ^VkPipelineViewportSwizzleStateCreateInfoNV;
     VkPipelineViewportSwizzleStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineViewportSwizzleStateCreateFlagsNV;
       viewportCount :T_uint32_t;
       pViewportSwizzles :P_VkViewportSwizzleNV;
     end;



const VK_EXT_discard_rectangles = 1;
const VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION = 1;
const VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME = 'VK_EXT_discard_rectangles';

type P_VkDiscardRectangleModeEXT = ^VkDiscardRectangleModeEXT;
     VkDiscardRectangleModeEXT = (
       VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = 0,
       VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = 1,
       VK_DISCARD_RECTANGLE_MODE_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkPipelineDiscardRectangleStateCreateFlagsEXT = ^VkPipelineDiscardRectangleStateCreateFlagsEXT;
     VkPipelineDiscardRectangleStateCreateFlagsEXT = VkFlags;
type P_VkPhysicalDeviceDiscardRectanglePropertiesEXT = ^VkPhysicalDeviceDiscardRectanglePropertiesEXT;
     VkPhysicalDeviceDiscardRectanglePropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxDiscardRectangles :T_uint32_t;
     end;

type P_VkPipelineDiscardRectangleStateCreateInfoEXT = ^VkPipelineDiscardRectangleStateCreateInfoEXT;
     VkPipelineDiscardRectangleStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineDiscardRectangleStateCreateFlagsEXT;
       discardRectangleMode :VkDiscardRectangleModeEXT;
       discardRectangleCount :T_uint32_t;
       pDiscardRectangles :P_VkRect2D;
     end;

type PFN_vkCmdSetDiscardRectangleEXT = procedure( commandBuffer_:VkCommandBuffer; firstDiscardRectangle_:T_uint32_t; discardRectangleCount_:T_uint32_t; const pDiscardRectangles_:P_VkRect2D );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetDiscardRectangleEXT(
    commandBuffer_:VkCommandBuffer;
    firstDiscardRectangle_:T_uint32_t;
    discardRectangleCount_:T_uint32_t;
    pDiscardRectangles_:P_VkRect2D ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_conservative_rasterization = 1;
const VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1;
const VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME = 'VK_EXT_conservative_rasterization';

type P_VkConservativeRasterizationModeEXT = ^VkConservativeRasterizationModeEXT;
     VkConservativeRasterizationModeEXT = (
       VK_CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT = 0,
       VK_CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT = 1,
       VK_CONSERVATIVE_RASTERIZATION_MODE_UNDERESTIMATE_EXT = 2,
       VK_CONSERVATIVE_RASTERIZATION_MODE_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkPipelineRasterizationConservativeStateCreateFlagsEXT = ^VkPipelineRasterizationConservativeStateCreateFlagsEXT;
     VkPipelineRasterizationConservativeStateCreateFlagsEXT = VkFlags;
type P_VkPhysicalDeviceConservativeRasterizationPropertiesEXT = ^VkPhysicalDeviceConservativeRasterizationPropertiesEXT;
     VkPhysicalDeviceConservativeRasterizationPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       primitiveOverestimationSize :T_float;
       maxExtraPrimitiveOverestimationSize :T_float;
       extraPrimitiveOverestimationSizeGranularity :T_float;
       primitiveUnderestimation :VkBool32;
       conservativePointAndLineRasterization :VkBool32;
       degenerateTrianglesRasterized :VkBool32;
       degenerateLinesRasterized :VkBool32;
       fullyCoveredFragmentShaderInputVariable :VkBool32;
       conservativeRasterizationPostDepthCoverage :VkBool32;
     end;

type P_VkPipelineRasterizationConservativeStateCreateInfoEXT = ^VkPipelineRasterizationConservativeStateCreateInfoEXT;
     VkPipelineRasterizationConservativeStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineRasterizationConservativeStateCreateFlagsEXT;
       conservativeRasterizationMode :VkConservativeRasterizationModeEXT;
       extraPrimitiveOverestimationSize :T_float;
     end;



const VK_EXT_depth_clip_enable = 1;
const VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION = 1;
const VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME = 'VK_EXT_depth_clip_enable';
type P_VkPipelineRasterizationDepthClipStateCreateFlagsEXT = ^VkPipelineRasterizationDepthClipStateCreateFlagsEXT;
     VkPipelineRasterizationDepthClipStateCreateFlagsEXT = VkFlags;
type P_VkPhysicalDeviceDepthClipEnableFeaturesEXT = ^VkPhysicalDeviceDepthClipEnableFeaturesEXT;
     VkPhysicalDeviceDepthClipEnableFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       depthClipEnable :VkBool32;
     end;

type P_VkPipelineRasterizationDepthClipStateCreateInfoEXT = ^VkPipelineRasterizationDepthClipStateCreateInfoEXT;
     VkPipelineRasterizationDepthClipStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineRasterizationDepthClipStateCreateFlagsEXT;
       depthClipEnable :VkBool32;
     end;



const VK_EXT_swapchain_colorspace = 1;
const VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 4;
const VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME = 'VK_EXT_swapchain_colorspace';


const VK_EXT_hdr_metadata = 1;
const VK_EXT_HDR_METADATA_SPEC_VERSION  = 2;
const VK_EXT_HDR_METADATA_EXTENSION_NAME = 'VK_EXT_hdr_metadata';
type P_VkXYColorEXT = ^VkXYColorEXT;
     VkXYColorEXT = record
       x :T_float;
       y :T_float;
     end;

type P_VkHdrMetadataEXT = ^VkHdrMetadataEXT;
     VkHdrMetadataEXT = record
       sType :VkStructureType;
       pNext :P_void;
       displayPrimaryRed :VkXYColorEXT;
       displayPrimaryGreen :VkXYColorEXT;
       displayPrimaryBlue :VkXYColorEXT;
       whitePoint :VkXYColorEXT;
       maxLuminance :T_float;
       minLuminance :T_float;
       maxContentLightLevel :T_float;
       maxFrameAverageLightLevel :T_float;
     end;

type PFN_vkSetHdrMetadataEXT = procedure( device_:VkDevice; swapchainCount_:T_uint32_t; const pSwapchains_:P_VkSwapchainKHR; const pMetadata_:P_VkHdrMetadataEXT );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkSetHdrMetadataEXT(
    device_:VkDevice;
    swapchainCount_:T_uint32_t;
    pSwapchains_:P_VkSwapchainKHR;
    pMetadata_:P_VkHdrMetadataEXT ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_external_memory_dma_buf = 1;
const VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1;
const VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = 'VK_EXT_external_memory_dma_buf';


const VK_EXT_queue_family_foreign = 1;
const VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = 1;
const VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = 'VK_EXT_queue_family_foreign';
const VK_QUEUE_FAMILY_FOREIGN_EXT       = UInt32( $FFFFFFFF )-2; {(~0U-2)}


const VK_EXT_debug_utils = 1;
type P_VkDebugUtilsMessengerEXT = ^VkDebugUtilsMessengerEXT;
     VkDebugUtilsMessengerEXT = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_EXT_DEBUG_UTILS_SPEC_VERSION   = 2;
const VK_EXT_DEBUG_UTILS_EXTENSION_NAME = 'VK_EXT_debug_utils';
type P_VkDebugUtilsMessengerCallbackDataFlagsEXT = ^VkDebugUtilsMessengerCallbackDataFlagsEXT;
     VkDebugUtilsMessengerCallbackDataFlagsEXT = VkFlags;

type P_VkDebugUtilsMessageSeverityFlagBitsEXT = ^VkDebugUtilsMessageSeverityFlagBitsEXT;
     VkDebugUtilsMessageSeverityFlagBitsEXT = (
       VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = $00000001,
       VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = $00000010,
       VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = $00000100,
       VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = $00001000,
       VK_DEBUG_UTILS_MESSAGE_SEVERITY_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );

type P_VkDebugUtilsMessageTypeFlagBitsEXT = ^VkDebugUtilsMessageTypeFlagBitsEXT;
     VkDebugUtilsMessageTypeFlagBitsEXT = (
       VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = $00000001,
       VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = $00000002,
       VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = $00000004,
       VK_DEBUG_UTILS_MESSAGE_TYPE_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkDebugUtilsMessageTypeFlagsEXT = ^VkDebugUtilsMessageTypeFlagsEXT;
     VkDebugUtilsMessageTypeFlagsEXT = VkFlags;
type P_VkDebugUtilsMessageSeverityFlagsEXT = ^VkDebugUtilsMessageSeverityFlagsEXT;
     VkDebugUtilsMessageSeverityFlagsEXT = VkFlags;
type P_VkDebugUtilsMessengerCreateFlagsEXT = ^VkDebugUtilsMessengerCreateFlagsEXT;
     VkDebugUtilsMessengerCreateFlagsEXT = VkFlags;
type P_VkDebugUtilsLabelEXT = ^VkDebugUtilsLabelEXT;
     VkDebugUtilsLabelEXT = record
       sType :VkStructureType;
       pNext :P_void;
       pLabelName :P_char;
       color :array [ 0..4-1 ] of T_float;
     end;

type P_VkDebugUtilsObjectNameInfoEXT = ^VkDebugUtilsObjectNameInfoEXT;
     VkDebugUtilsObjectNameInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       objectType :VkObjectType;
       objectHandle :T_uint64_t;
       pObjectName :P_char;
     end;

type P_VkDebugUtilsMessengerCallbackDataEXT = ^VkDebugUtilsMessengerCallbackDataEXT;
     VkDebugUtilsMessengerCallbackDataEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDebugUtilsMessengerCallbackDataFlagsEXT;
       pMessageIdName :P_char;
       messageIdNumber :T_int32_t;
       pMessage :P_char;
       queueLabelCount :T_uint32_t;
       pQueueLabels :P_VkDebugUtilsLabelEXT;
       cmdBufLabelCount :T_uint32_t;
       pCmdBufLabels :P_VkDebugUtilsLabelEXT;
       objectCount :T_uint32_t;
       pObjects :P_VkDebugUtilsObjectNameInfoEXT;
     end;

type PFN_vkDebugUtilsMessengerCallbackEXT = function(
    messageSeverity_:VkDebugUtilsMessageSeverityFlagBitsEXT;
    messageTypes_:VkDebugUtilsMessageTypeFlagsEXT;
    pCallbackData_:P_VkDebugUtilsMessengerCallbackDataEXT;
    pUserData_:P_void ) :VkBool32;

type P_VkDebugUtilsMessengerCreateInfoEXT = ^VkDebugUtilsMessengerCreateInfoEXT;
     VkDebugUtilsMessengerCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDebugUtilsMessengerCreateFlagsEXT;
       messageSeverity :VkDebugUtilsMessageSeverityFlagsEXT;
       messageType :VkDebugUtilsMessageTypeFlagsEXT;
       pfnUserCallback :PFN_vkDebugUtilsMessengerCallbackEXT;
       pUserData :P_void;
     end;

type P_VkDebugUtilsObjectTagInfoEXT = ^VkDebugUtilsObjectTagInfoEXT;
     VkDebugUtilsObjectTagInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       objectType :VkObjectType;
       objectHandle :T_uint64_t;
       tagName :T_uint64_t;
       tagSize :T_size_t;
       pTag :P_void;
     end;

type PFN_vkSetDebugUtilsObjectNameEXT = function( device_:VkDevice; const pNameInfo_:P_VkDebugUtilsObjectNameInfoEXT ) :VkResult;
type PFN_vkSetDebugUtilsObjectTagEXT = function( device_:VkDevice; const pTagInfo_:P_VkDebugUtilsObjectTagInfoEXT ) :VkResult;
type PFN_vkQueueBeginDebugUtilsLabelEXT = procedure( queue_:VkQueue; const pLabelInfo_:P_VkDebugUtilsLabelEXT );
type PFN_vkQueueEndDebugUtilsLabelEXT = procedure( queue_:VkQueue );
type PFN_vkQueueInsertDebugUtilsLabelEXT = procedure( queue_:VkQueue; const pLabelInfo_:P_VkDebugUtilsLabelEXT );
type PFN_vkCmdBeginDebugUtilsLabelEXT = procedure( commandBuffer_:VkCommandBuffer; const pLabelInfo_:P_VkDebugUtilsLabelEXT );
type PFN_vkCmdEndDebugUtilsLabelEXT = procedure( commandBuffer_:VkCommandBuffer );
type PFN_vkCmdInsertDebugUtilsLabelEXT = procedure( commandBuffer_:VkCommandBuffer; const pLabelInfo_:P_VkDebugUtilsLabelEXT );
type PFN_vkCreateDebugUtilsMessengerEXT = function( instance_:VkInstance; const pCreateInfo_:P_VkDebugUtilsMessengerCreateInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pMessenger_:P_VkDebugUtilsMessengerEXT ) :VkResult;
type PFN_vkDestroyDebugUtilsMessengerEXT = procedure( instance_:VkInstance; messenger_:VkDebugUtilsMessengerEXT; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkSubmitDebugUtilsMessageEXT = procedure( instance_:VkInstance; messageSeverity_:VkDebugUtilsMessageSeverityFlagBitsEXT; messageTypes_:VkDebugUtilsMessageTypeFlagsEXT; const pCallbackData_:P_VkDebugUtilsMessengerCallbackDataEXT );

{$IFNDEF VK_NO_PROTOTYPES }
function vkSetDebugUtilsObjectNameEXT(
    device_:VkDevice;
    pNameInfo_:P_VkDebugUtilsObjectNameInfoEXT ) :VkResult; stdcall; external DLLNAME;

function vkSetDebugUtilsObjectTagEXT(
    device_:VkDevice;
    pTagInfo_:P_VkDebugUtilsObjectTagInfoEXT ) :VkResult; stdcall; external DLLNAME;

procedure vkQueueBeginDebugUtilsLabelEXT(
    queue_:VkQueue;
    pLabelInfo_:P_VkDebugUtilsLabelEXT ); stdcall; external DLLNAME;

procedure vkQueueEndDebugUtilsLabelEXT(
    queue_:VkQueue ); stdcall; external DLLNAME;

procedure vkQueueInsertDebugUtilsLabelEXT(
    queue_:VkQueue;
    pLabelInfo_:P_VkDebugUtilsLabelEXT ); stdcall; external DLLNAME;

procedure vkCmdBeginDebugUtilsLabelEXT(
    commandBuffer_:VkCommandBuffer;
    pLabelInfo_:P_VkDebugUtilsLabelEXT ); stdcall; external DLLNAME;

procedure vkCmdEndDebugUtilsLabelEXT(
    commandBuffer_:VkCommandBuffer ); stdcall; external DLLNAME;

procedure vkCmdInsertDebugUtilsLabelEXT(
    commandBuffer_:VkCommandBuffer;
    pLabelInfo_:P_VkDebugUtilsLabelEXT ); stdcall; external DLLNAME;

function vkCreateDebugUtilsMessengerEXT(
    instance_:VkInstance;
    pCreateInfo_:P_VkDebugUtilsMessengerCreateInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pMessenger_:P_VkDebugUtilsMessengerEXT ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyDebugUtilsMessengerEXT(
    instance_:VkInstance;
    messenger_:VkDebugUtilsMessengerEXT;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkSubmitDebugUtilsMessageEXT(
    instance_:VkInstance;
    messageSeverity_:VkDebugUtilsMessageSeverityFlagBitsEXT;
    messageTypes_:VkDebugUtilsMessageTypeFlagsEXT;
    pCallbackData_:P_VkDebugUtilsMessengerCallbackDataEXT ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_sampler_filter_minmax = 1;
const VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 2;
const VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = 'VK_EXT_sampler_filter_minmax';
type P_VkSamplerReductionModeEXT = ^VkSamplerReductionModeEXT;
     VkSamplerReductionModeEXT = VkSamplerReductionMode;

type P_VkSamplerReductionModeCreateInfoEXT = ^VkSamplerReductionModeCreateInfoEXT;
     VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfo;

type P_VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT = ^VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT;
     VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT = VkPhysicalDeviceSamplerFilterMinmaxProperties;



const VK_AMD_gpu_shader_int16 = 1;
const VK_AMD_GPU_SHADER_INT16_SPEC_VERSION = 2;
const VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME = 'VK_AMD_gpu_shader_int16';


const VK_AMD_mixed_attachment_samples = 1;
const VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1;
const VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = 'VK_AMD_mixed_attachment_samples';


const VK_AMD_shader_fragment_mask = 1;
const VK_AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION = 1;
const VK_AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME = 'VK_AMD_shader_fragment_mask';


const VK_EXT_inline_uniform_block = 1;
const VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1;
const VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = 'VK_EXT_inline_uniform_block';
type P_VkPhysicalDeviceInlineUniformBlockFeaturesEXT = ^VkPhysicalDeviceInlineUniformBlockFeaturesEXT;
     VkPhysicalDeviceInlineUniformBlockFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       inlineUniformBlock :VkBool32;
       descriptorBindingInlineUniformBlockUpdateAfterBind :VkBool32;
     end;

type P_VkPhysicalDeviceInlineUniformBlockPropertiesEXT = ^VkPhysicalDeviceInlineUniformBlockPropertiesEXT;
     VkPhysicalDeviceInlineUniformBlockPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxInlineUniformBlockSize :T_uint32_t;
       maxPerStageDescriptorInlineUniformBlocks :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :T_uint32_t;
       maxDescriptorSetInlineUniformBlocks :T_uint32_t;
       maxDescriptorSetUpdateAfterBindInlineUniformBlocks :T_uint32_t;
     end;

type P_VkWriteDescriptorSetInlineUniformBlockEXT = ^VkWriteDescriptorSetInlineUniformBlockEXT;
     VkWriteDescriptorSetInlineUniformBlockEXT = record
       sType :VkStructureType;
       pNext :P_void;
       dataSize :T_uint32_t;
       pData :P_void;
     end;

type P_VkDescriptorPoolInlineUniformBlockCreateInfoEXT = ^VkDescriptorPoolInlineUniformBlockCreateInfoEXT;
     VkDescriptorPoolInlineUniformBlockCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxInlineUniformBlockBindings :T_uint32_t;
     end;



const VK_EXT_shader_stencil_export = 1;
const VK_EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION = 1;
const VK_EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME = 'VK_EXT_shader_stencil_export';


const VK_EXT_sample_locations = 1;
const VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1;
const VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = 'VK_EXT_sample_locations';
type P_VkSampleLocationEXT = ^VkSampleLocationEXT;
     VkSampleLocationEXT = record
       x :T_float;
       y :T_float;
     end;

type P_VkSampleLocationsInfoEXT = ^VkSampleLocationsInfoEXT;
     VkSampleLocationsInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       sampleLocationsPerPixel :VkSampleCountFlagBits;
       sampleLocationGridSize :VkExtent2D;
       sampleLocationsCount :T_uint32_t;
       pSampleLocations :P_VkSampleLocationEXT;
     end;

type P_VkAttachmentSampleLocationsEXT = ^VkAttachmentSampleLocationsEXT;
     VkAttachmentSampleLocationsEXT = record
       attachmentIndex :T_uint32_t;
       sampleLocationsInfo :VkSampleLocationsInfoEXT;
     end;

type P_VkSubpassSampleLocationsEXT = ^VkSubpassSampleLocationsEXT;
     VkSubpassSampleLocationsEXT = record
       subpassIndex :T_uint32_t;
       sampleLocationsInfo :VkSampleLocationsInfoEXT;
     end;

type P_VkRenderPassSampleLocationsBeginInfoEXT = ^VkRenderPassSampleLocationsBeginInfoEXT;
     VkRenderPassSampleLocationsBeginInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       attachmentInitialSampleLocationsCount :T_uint32_t;
       pAttachmentInitialSampleLocations :P_VkAttachmentSampleLocationsEXT;
       postSubpassSampleLocationsCount :T_uint32_t;
       pPostSubpassSampleLocations :P_VkSubpassSampleLocationsEXT;
     end;

type P_VkPipelineSampleLocationsStateCreateInfoEXT = ^VkPipelineSampleLocationsStateCreateInfoEXT;
     VkPipelineSampleLocationsStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       sampleLocationsEnable :VkBool32;
       sampleLocationsInfo :VkSampleLocationsInfoEXT;
     end;

type P_VkPhysicalDeviceSampleLocationsPropertiesEXT = ^VkPhysicalDeviceSampleLocationsPropertiesEXT;
     VkPhysicalDeviceSampleLocationsPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       sampleLocationSampleCounts :VkSampleCountFlags;
       maxSampleLocationGridSize :VkExtent2D;
       sampleLocationCoordinateRange :array [ 0..2-1 ] of T_float;
       sampleLocationSubPixelBits :T_uint32_t;
       variableSampleLocations :VkBool32;
     end;

type P_VkMultisamplePropertiesEXT = ^VkMultisamplePropertiesEXT;
     VkMultisamplePropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxSampleLocationGridSize :VkExtent2D;
     end;

type PFN_vkCmdSetSampleLocationsEXT = procedure( commandBuffer_:VkCommandBuffer; const pSampleLocationsInfo_:P_VkSampleLocationsInfoEXT );
type PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT = procedure( physicalDevice_:VkPhysicalDevice; samples_:VkSampleCountFlagBits; pMultisampleProperties_:P_VkMultisamplePropertiesEXT );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetSampleLocationsEXT(
    commandBuffer_:VkCommandBuffer;
    pSampleLocationsInfo_:P_VkSampleLocationsInfoEXT ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceMultisamplePropertiesEXT(
    physicalDevice_:VkPhysicalDevice;
    samples_:VkSampleCountFlagBits;
    pMultisampleProperties_:P_VkMultisamplePropertiesEXT ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_blend_operation_advanced = 1;
const VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2;
const VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = 'VK_EXT_blend_operation_advanced';

type P_VkBlendOverlapEXT = ^VkBlendOverlapEXT;
     VkBlendOverlapEXT = (
       VK_BLEND_OVERLAP_UNCORRELATED_EXT = 0,
       VK_BLEND_OVERLAP_DISJOINT_EXT = 1,
       VK_BLEND_OVERLAP_CONJOINT_EXT = 2,
       VK_BLEND_OVERLAP_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = ^VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
     VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       advancedBlendCoherentOperations :VkBool32;
     end;

type P_VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = ^VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT;
     VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       advancedBlendMaxColorAttachments :T_uint32_t;
       advancedBlendIndependentBlend :VkBool32;
       advancedBlendNonPremultipliedSrcColor :VkBool32;
       advancedBlendNonPremultipliedDstColor :VkBool32;
       advancedBlendCorrelatedOverlap :VkBool32;
       advancedBlendAllOperations :VkBool32;
     end;

type P_VkPipelineColorBlendAdvancedStateCreateInfoEXT = ^VkPipelineColorBlendAdvancedStateCreateInfoEXT;
     VkPipelineColorBlendAdvancedStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       srcPremultiplied :VkBool32;
       dstPremultiplied :VkBool32;
       blendOverlap :VkBlendOverlapEXT;
     end;



const VK_NV_fragment_coverage_to_color = 1;
const VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1;
const VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME = 'VK_NV_fragment_coverage_to_color';
type P_VkPipelineCoverageToColorStateCreateFlagsNV = ^VkPipelineCoverageToColorStateCreateFlagsNV;
     VkPipelineCoverageToColorStateCreateFlagsNV = VkFlags;
type P_VkPipelineCoverageToColorStateCreateInfoNV = ^VkPipelineCoverageToColorStateCreateInfoNV;
     VkPipelineCoverageToColorStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineCoverageToColorStateCreateFlagsNV;
       coverageToColorEnable :VkBool32;
       coverageToColorLocation :T_uint32_t;
     end;



const VK_NV_framebuffer_mixed_samples = 1;
const VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1;
const VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME = 'VK_NV_framebuffer_mixed_samples';

type P_VkCoverageModulationModeNV = ^VkCoverageModulationModeNV;
     VkCoverageModulationModeNV = (
       VK_COVERAGE_MODULATION_MODE_NONE_NV = 0,
       VK_COVERAGE_MODULATION_MODE_RGB_NV = 1,
       VK_COVERAGE_MODULATION_MODE_ALPHA_NV = 2,
       VK_COVERAGE_MODULATION_MODE_RGBA_NV = 3,
       VK_COVERAGE_MODULATION_MODE_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkPipelineCoverageModulationStateCreateFlagsNV = ^VkPipelineCoverageModulationStateCreateFlagsNV;
     VkPipelineCoverageModulationStateCreateFlagsNV = VkFlags;
type P_VkPipelineCoverageModulationStateCreateInfoNV = ^VkPipelineCoverageModulationStateCreateInfoNV;
     VkPipelineCoverageModulationStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineCoverageModulationStateCreateFlagsNV;
       coverageModulationMode :VkCoverageModulationModeNV;
       coverageModulationTableEnable :VkBool32;
       coverageModulationTableCount :T_uint32_t;
       pCoverageModulationTable :P_float;
     end;



const VK_NV_fill_rectangle = 1;
const VK_NV_FILL_RECTANGLE_SPEC_VERSION = 1;
const VK_NV_FILL_RECTANGLE_EXTENSION_NAME = 'VK_NV_fill_rectangle';


const VK_NV_shader_sm_builtins = 1;
const VK_NV_SHADER_SM_BUILTINS_SPEC_VERSION = 1;
const VK_NV_SHADER_SM_BUILTINS_EXTENSION_NAME = 'VK_NV_shader_sm_builtins';
type P_VkPhysicalDeviceShaderSMBuiltinsPropertiesNV = ^VkPhysicalDeviceShaderSMBuiltinsPropertiesNV;
     VkPhysicalDeviceShaderSMBuiltinsPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       shaderSMCount :T_uint32_t;
       shaderWarpsPerSM :T_uint32_t;
     end;

type P_VkPhysicalDeviceShaderSMBuiltinsFeaturesNV = ^VkPhysicalDeviceShaderSMBuiltinsFeaturesNV;
     VkPhysicalDeviceShaderSMBuiltinsFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       shaderSMBuiltins :VkBool32;
     end;



const VK_EXT_post_depth_coverage = 1;
const VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1;
const VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME = 'VK_EXT_post_depth_coverage';


const VK_EXT_image_drm_format_modifier = 1;
const VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1;
const VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME = 'VK_EXT_image_drm_format_modifier';
type P_VkDrmFormatModifierPropertiesEXT = ^VkDrmFormatModifierPropertiesEXT;
     VkDrmFormatModifierPropertiesEXT = record
       drmFormatModifier :T_uint64_t;
       drmFormatModifierPlaneCount :T_uint32_t;
       drmFormatModifierTilingFeatures :VkFormatFeatureFlags;
     end;

type P_VkDrmFormatModifierPropertiesListEXT = ^VkDrmFormatModifierPropertiesListEXT;
     VkDrmFormatModifierPropertiesListEXT = record
       sType :VkStructureType;
       pNext :P_void;
       drmFormatModifierCount :T_uint32_t;
       pDrmFormatModifierProperties :P_VkDrmFormatModifierPropertiesEXT;
     end;

type P_VkPhysicalDeviceImageDrmFormatModifierInfoEXT = ^VkPhysicalDeviceImageDrmFormatModifierInfoEXT;
     VkPhysicalDeviceImageDrmFormatModifierInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       drmFormatModifier :T_uint64_t;
       sharingMode :VkSharingMode;
       queueFamilyIndexCount :T_uint32_t;
       pQueueFamilyIndices :P_uint32_t;
     end;

type P_VkImageDrmFormatModifierListCreateInfoEXT = ^VkImageDrmFormatModifierListCreateInfoEXT;
     VkImageDrmFormatModifierListCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       drmFormatModifierCount :T_uint32_t;
       pDrmFormatModifiers :P_uint64_t;
     end;

type P_VkImageDrmFormatModifierExplicitCreateInfoEXT = ^VkImageDrmFormatModifierExplicitCreateInfoEXT;
     VkImageDrmFormatModifierExplicitCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       drmFormatModifier :T_uint64_t;
       drmFormatModifierPlaneCount :T_uint32_t;
       pPlaneLayouts :P_VkSubresourceLayout;
     end;

type P_VkImageDrmFormatModifierPropertiesEXT = ^VkImageDrmFormatModifierPropertiesEXT;
     VkImageDrmFormatModifierPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       drmFormatModifier :T_uint64_t;
     end;

type PFN_vkGetImageDrmFormatModifierPropertiesEXT = function( device_:VkDevice; image_:VkImage; pProperties_:P_VkImageDrmFormatModifierPropertiesEXT ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetImageDrmFormatModifierPropertiesEXT(
    device_:VkDevice;
    image_:VkImage;
    pProperties_:P_VkImageDrmFormatModifierPropertiesEXT ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_validation_cache = 1;
type P_VkValidationCacheEXT = ^VkValidationCacheEXT;
     VkValidationCacheEXT = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_EXT_VALIDATION_CACHE_SPEC_VERSION = 1;
const VK_EXT_VALIDATION_CACHE_EXTENSION_NAME = 'VK_EXT_validation_cache';

type P_VkValidationCacheHeaderVersionEXT = ^VkValidationCacheHeaderVersionEXT;
     VkValidationCacheHeaderVersionEXT = (
       VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = 1,
       VK_VALIDATION_CACHE_HEADER_VERSION_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkValidationCacheCreateFlagsEXT = ^VkValidationCacheCreateFlagsEXT;
     VkValidationCacheCreateFlagsEXT = VkFlags;
type P_VkValidationCacheCreateInfoEXT = ^VkValidationCacheCreateInfoEXT;
     VkValidationCacheCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkValidationCacheCreateFlagsEXT;
       initialDataSize :T_size_t;
       pInitialData :P_void;
     end;

type P_VkShaderModuleValidationCacheCreateInfoEXT = ^VkShaderModuleValidationCacheCreateInfoEXT;
     VkShaderModuleValidationCacheCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       validationCache :VkValidationCacheEXT;
     end;

type PFN_vkCreateValidationCacheEXT = function( device_:VkDevice; const pCreateInfo_:P_VkValidationCacheCreateInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pValidationCache_:P_VkValidationCacheEXT ) :VkResult;
type PFN_vkDestroyValidationCacheEXT = procedure( device_:VkDevice; validationCache_:VkValidationCacheEXT; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkMergeValidationCachesEXT = function( device_:VkDevice; dstCache_:VkValidationCacheEXT; srcCacheCount_:T_uint32_t; const pSrcCaches_:P_VkValidationCacheEXT ) :VkResult;
type PFN_vkGetValidationCacheDataEXT = function( device_:VkDevice; validationCache_:VkValidationCacheEXT; pDataSize_:P_size_t; pData_:P_void ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateValidationCacheEXT(
    device_:VkDevice;
    pCreateInfo_:P_VkValidationCacheCreateInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pValidationCache_:P_VkValidationCacheEXT ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyValidationCacheEXT(
    device_:VkDevice;
    validationCache_:VkValidationCacheEXT;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkMergeValidationCachesEXT(
    device_:VkDevice;
    dstCache_:VkValidationCacheEXT;
    srcCacheCount_:T_uint32_t;
    pSrcCaches_:P_VkValidationCacheEXT ) :VkResult; stdcall; external DLLNAME;

function vkGetValidationCacheDataEXT(
    device_:VkDevice;
    validationCache_:VkValidationCacheEXT;
    pDataSize_:P_size_t;
    pData_:P_void ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_descriptor_indexing = 1;
const VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2;
const VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = 'VK_EXT_descriptor_indexing';
type P_VkDescriptorBindingFlagBitsEXT = ^VkDescriptorBindingFlagBitsEXT;
     VkDescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBits;

type P_VkDescriptorBindingFlagsEXT = ^VkDescriptorBindingFlagsEXT;
     VkDescriptorBindingFlagsEXT = VkDescriptorBindingFlags;

type P_VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = ^VkDescriptorSetLayoutBindingFlagsCreateInfoEXT;
     VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfo;

type P_VkPhysicalDeviceDescriptorIndexingFeaturesEXT = ^VkPhysicalDeviceDescriptorIndexingFeaturesEXT;
     VkPhysicalDeviceDescriptorIndexingFeaturesEXT = VkPhysicalDeviceDescriptorIndexingFeatures;

type P_VkPhysicalDeviceDescriptorIndexingPropertiesEXT = ^VkPhysicalDeviceDescriptorIndexingPropertiesEXT;
     VkPhysicalDeviceDescriptorIndexingPropertiesEXT = VkPhysicalDeviceDescriptorIndexingProperties;

type P_VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = ^VkDescriptorSetVariableDescriptorCountAllocateInfoEXT;
     VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfo;

type P_VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = ^VkDescriptorSetVariableDescriptorCountLayoutSupportEXT;
     VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupport;



const VK_EXT_shader_viewport_index_layer = 1;
const VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION = 1;
const VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME = 'VK_EXT_shader_viewport_index_layer';


const VK_NV_shading_rate_image = 1;
const VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3;
const VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME = 'VK_NV_shading_rate_image';

type P_VkShadingRatePaletteEntryNV = ^VkShadingRatePaletteEntryNV;
     VkShadingRatePaletteEntryNV = (
       VK_SHADING_RATE_PALETTE_ENTRY_NO_INVOCATIONS_NV = 0,
       VK_SHADING_RATE_PALETTE_ENTRY_16_INVOCATIONS_PER_PIXEL_NV = 1,
       VK_SHADING_RATE_PALETTE_ENTRY_8_INVOCATIONS_PER_PIXEL_NV = 2,
       VK_SHADING_RATE_PALETTE_ENTRY_4_INVOCATIONS_PER_PIXEL_NV = 3,
       VK_SHADING_RATE_PALETTE_ENTRY_2_INVOCATIONS_PER_PIXEL_NV = 4,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_PIXEL_NV = 5,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X1_PIXELS_NV = 6,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_1X2_PIXELS_NV = 7,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X2_PIXELS_NV = 8,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X2_PIXELS_NV = 9,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_2X4_PIXELS_NV = 10,
       VK_SHADING_RATE_PALETTE_ENTRY_1_INVOCATION_PER_4X4_PIXELS_NV = 11,
       VK_SHADING_RATE_PALETTE_ENTRY_MAX_ENUM_NV = $7FFFFFFF
     );

type P_VkCoarseSampleOrderTypeNV = ^VkCoarseSampleOrderTypeNV;
     VkCoarseSampleOrderTypeNV = (
       VK_COARSE_SAMPLE_ORDER_TYPE_DEFAULT_NV = 0,
       VK_COARSE_SAMPLE_ORDER_TYPE_CUSTOM_NV = 1,
       VK_COARSE_SAMPLE_ORDER_TYPE_PIXEL_MAJOR_NV = 2,
       VK_COARSE_SAMPLE_ORDER_TYPE_SAMPLE_MAJOR_NV = 3,
       VK_COARSE_SAMPLE_ORDER_TYPE_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkShadingRatePaletteNV = ^VkShadingRatePaletteNV;
     VkShadingRatePaletteNV = record
       shadingRatePaletteEntryCount :T_uint32_t;
       pShadingRatePaletteEntries :P_VkShadingRatePaletteEntryNV;
     end;

type P_VkPipelineViewportShadingRateImageStateCreateInfoNV = ^VkPipelineViewportShadingRateImageStateCreateInfoNV;
     VkPipelineViewportShadingRateImageStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       shadingRateImageEnable :VkBool32;
       viewportCount :T_uint32_t;
       pShadingRatePalettes :P_VkShadingRatePaletteNV;
     end;

type P_VkPhysicalDeviceShadingRateImageFeaturesNV = ^VkPhysicalDeviceShadingRateImageFeaturesNV;
     VkPhysicalDeviceShadingRateImageFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       shadingRateImage :VkBool32;
       shadingRateCoarseSampleOrder :VkBool32;
     end;

type P_VkPhysicalDeviceShadingRateImagePropertiesNV = ^VkPhysicalDeviceShadingRateImagePropertiesNV;
     VkPhysicalDeviceShadingRateImagePropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       shadingRateTexelSize :VkExtent2D;
       shadingRatePaletteSize :T_uint32_t;
       shadingRateMaxCoarseSamples :T_uint32_t;
     end;

type P_VkCoarseSampleLocationNV = ^VkCoarseSampleLocationNV;
     VkCoarseSampleLocationNV = record
       pixelX :T_uint32_t;
       pixelY :T_uint32_t;
       sample :T_uint32_t;
     end;

type P_VkCoarseSampleOrderCustomNV = ^VkCoarseSampleOrderCustomNV;
     VkCoarseSampleOrderCustomNV = record
       shadingRate :VkShadingRatePaletteEntryNV;
       sampleCount :T_uint32_t;
       sampleLocationCount :T_uint32_t;
       pSampleLocations :P_VkCoarseSampleLocationNV;
     end;

type P_VkPipelineViewportCoarseSampleOrderStateCreateInfoNV = ^VkPipelineViewportCoarseSampleOrderStateCreateInfoNV;
     VkPipelineViewportCoarseSampleOrderStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       sampleOrderType :VkCoarseSampleOrderTypeNV;
       customSampleOrderCount :T_uint32_t;
       pCustomSampleOrders :P_VkCoarseSampleOrderCustomNV;
     end;

type PFN_vkCmdBindShadingRateImageNV = procedure( commandBuffer_:VkCommandBuffer; imageView_:VkImageView; imageLayout_:VkImageLayout );
type PFN_vkCmdSetViewportShadingRatePaletteNV = procedure( commandBuffer_:VkCommandBuffer; firstViewport_:T_uint32_t; viewportCount_:T_uint32_t; const pShadingRatePalettes_:P_VkShadingRatePaletteNV );
type PFN_vkCmdSetCoarseSampleOrderNV = procedure( commandBuffer_:VkCommandBuffer; sampleOrderType_:VkCoarseSampleOrderTypeNV; customSampleOrderCount_:T_uint32_t; const pCustomSampleOrders_:P_VkCoarseSampleOrderCustomNV );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdBindShadingRateImageNV(
    commandBuffer_:VkCommandBuffer;
    imageView_:VkImageView;
    imageLayout_:VkImageLayout ); stdcall; external DLLNAME;

procedure vkCmdSetViewportShadingRatePaletteNV(
    commandBuffer_:VkCommandBuffer;
    firstViewport_:T_uint32_t;
    viewportCount_:T_uint32_t;
    pShadingRatePalettes_:P_VkShadingRatePaletteNV ); stdcall; external DLLNAME;

procedure vkCmdSetCoarseSampleOrderNV(
    commandBuffer_:VkCommandBuffer;
    sampleOrderType_:VkCoarseSampleOrderTypeNV;
    customSampleOrderCount_:T_uint32_t;
    pCustomSampleOrders_:P_VkCoarseSampleOrderCustomNV ); stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_ray_tracing = 1;
type P_VkAccelerationStructureNV = ^VkAccelerationStructureNV;
     VkAccelerationStructureNV = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_NV_RAY_TRACING_SPEC_VERSION    = 3;
const VK_NV_RAY_TRACING_EXTENSION_NAME = 'VK_NV_ray_tracing';
const VK_SHADER_UNUSED_KHR              = UInt32( $FFFFFFFF ); {(~0U)}
const VK_SHADER_UNUSED_NV               = VK_SHADER_UNUSED_KHR;

type P_VkRayTracingShaderGroupTypeKHR = ^VkRayTracingShaderGroupTypeKHR;
     VkRayTracingShaderGroupTypeKHR = (
       VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR = 0,
       VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR = 1,
       VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR = 2,
       VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR,
       VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR,
       VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR,
       VK_RAY_TRACING_SHADER_GROUP_TYPE_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkRayTracingShaderGroupTypeNV = ^VkRayTracingShaderGroupTypeNV;
     VkRayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeKHR;


type P_VkGeometryTypeKHR = ^VkGeometryTypeKHR;
     VkGeometryTypeKHR = (
       VK_GEOMETRY_TYPE_TRIANGLES_KHR = 0,
       VK_GEOMETRY_TYPE_AABBS_KHR = 1,
       VK_GEOMETRY_TYPE_INSTANCES_KHR = 2,
       VK_GEOMETRY_TYPE_TRIANGLES_NV = VK_GEOMETRY_TYPE_TRIANGLES_KHR,
       VK_GEOMETRY_TYPE_AABBS_NV = VK_GEOMETRY_TYPE_AABBS_KHR,
       VK_GEOMETRY_TYPE_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkGeometryTypeNV = ^VkGeometryTypeNV;
     VkGeometryTypeNV = VkGeometryTypeKHR;


type P_VkAccelerationStructureTypeKHR = ^VkAccelerationStructureTypeKHR;
     VkAccelerationStructureTypeKHR = (
       VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR = 0,
       VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR = 1,
       VK_ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR = 2,
       VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR,
       VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR,
       VK_ACCELERATION_STRUCTURE_TYPE_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkAccelerationStructureTypeNV = ^VkAccelerationStructureTypeNV;
     VkAccelerationStructureTypeNV = VkAccelerationStructureTypeKHR;


type P_VkCopyAccelerationStructureModeKHR = ^VkCopyAccelerationStructureModeKHR;
     VkCopyAccelerationStructureModeKHR = (
       VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR = 0,
       VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR = 1,
       VK_COPY_ACCELERATION_STRUCTURE_MODE_SERIALIZE_KHR = 2,
       VK_COPY_ACCELERATION_STRUCTURE_MODE_DESERIALIZE_KHR = 3,
       VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR,
       VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR,
       VK_COPY_ACCELERATION_STRUCTURE_MODE_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkCopyAccelerationStructureModeNV = ^VkCopyAccelerationStructureModeNV;
     VkCopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeKHR;


type P_VkAccelerationStructureMemoryRequirementsTypeNV = ^VkAccelerationStructureMemoryRequirementsTypeNV;
     VkAccelerationStructureMemoryRequirementsTypeNV = (
       VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = 0,
       VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = 1,
       VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = 2,
       VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_MAX_ENUM_NV = $7FFFFFFF
     );

type P_VkGeometryFlagBitsKHR = ^VkGeometryFlagBitsKHR;
     VkGeometryFlagBitsKHR = (
       VK_GEOMETRY_OPAQUE_BIT_KHR = $00000001,
       VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR = $00000002,
       VK_GEOMETRY_OPAQUE_BIT_NV = VK_GEOMETRY_OPAQUE_BIT_KHR,
       VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR,
       VK_GEOMETRY_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkGeometryFlagsKHR = ^VkGeometryFlagsKHR;
     VkGeometryFlagsKHR = VkFlags;
type P_VkGeometryFlagsNV = ^VkGeometryFlagsNV;
     VkGeometryFlagsNV = VkGeometryFlagsKHR;

type P_VkGeometryFlagBitsNV = ^VkGeometryFlagBitsNV;
     VkGeometryFlagBitsNV = VkGeometryFlagBitsKHR;


type P_VkGeometryInstanceFlagBitsKHR = ^VkGeometryInstanceFlagBitsKHR;
     VkGeometryInstanceFlagBitsKHR = (
       VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR = $00000001,
       VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR = $00000002,
       VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR = $00000004,
       VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR = $00000008,
       VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR,
       VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR,
       VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR,
       VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR,
       VK_GEOMETRY_INSTANCE_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkGeometryInstanceFlagsKHR = ^VkGeometryInstanceFlagsKHR;
     VkGeometryInstanceFlagsKHR = VkFlags;
type P_VkGeometryInstanceFlagsNV = ^VkGeometryInstanceFlagsNV;
     VkGeometryInstanceFlagsNV = VkGeometryInstanceFlagsKHR;

type P_VkGeometryInstanceFlagBitsNV = ^VkGeometryInstanceFlagBitsNV;
     VkGeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsKHR;


type P_VkBuildAccelerationStructureFlagBitsKHR = ^VkBuildAccelerationStructureFlagBitsKHR;
     VkBuildAccelerationStructureFlagBitsKHR = (
       VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR = $00000001,
       VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR = $00000002,
       VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR = $00000004,
       VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR = $00000008,
       VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR = $00000010,
       VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR,
       VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR,
       VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR,
       VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR,
       VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR,
       VK_BUILD_ACCELERATION_STRUCTURE_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkBuildAccelerationStructureFlagsKHR = ^VkBuildAccelerationStructureFlagsKHR;
     VkBuildAccelerationStructureFlagsKHR = VkFlags;
type P_VkBuildAccelerationStructureFlagsNV = ^VkBuildAccelerationStructureFlagsNV;
     VkBuildAccelerationStructureFlagsNV = VkBuildAccelerationStructureFlagsKHR;

type P_VkBuildAccelerationStructureFlagBitsNV = ^VkBuildAccelerationStructureFlagBitsNV;
     VkBuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsKHR;

type P_VkRayTracingShaderGroupCreateInfoNV = ^VkRayTracingShaderGroupCreateInfoNV;
     VkRayTracingShaderGroupCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkRayTracingShaderGroupTypeKHR;
       generalShader :T_uint32_t;
       closestHitShader :T_uint32_t;
       anyHitShader :T_uint32_t;
       intersectionShader :T_uint32_t;
     end;

type P_VkRayTracingPipelineCreateInfoNV = ^VkRayTracingPipelineCreateInfoNV;
     VkRayTracingPipelineCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineCreateFlags;
       stageCount :T_uint32_t;
       pStages :P_VkPipelineShaderStageCreateInfo;
       groupCount :T_uint32_t;
       pGroups :P_VkRayTracingShaderGroupCreateInfoNV;
       maxRecursionDepth :T_uint32_t;
       layout :VkPipelineLayout;
       basePipelineHandle :VkPipeline;
       basePipelineIndex :T_int32_t;
     end;

type P_VkGeometryTrianglesNV = ^VkGeometryTrianglesNV;
     VkGeometryTrianglesNV = record
       sType :VkStructureType;
       pNext :P_void;
       vertexData :VkBuffer;
       vertexOffset :VkDeviceSize;
       vertexCount :T_uint32_t;
       vertexStride :VkDeviceSize;
       vertexFormat :VkFormat;
       indexData :VkBuffer;
       indexOffset :VkDeviceSize;
       indexCount :T_uint32_t;
       indexType :VkIndexType;
       transformData :VkBuffer;
       transformOffset :VkDeviceSize;
     end;

type P_VkGeometryAABBNV = ^VkGeometryAABBNV;
     VkGeometryAABBNV = record
       sType :VkStructureType;
       pNext :P_void;
       aabbData :VkBuffer;
       numAABBs :T_uint32_t;
       stride :T_uint32_t;
       offset :VkDeviceSize;
     end;

type P_VkGeometryDataNV = ^VkGeometryDataNV;
     VkGeometryDataNV = record
       triangles :VkGeometryTrianglesNV;
       aabbs :VkGeometryAABBNV;
     end;

type P_VkGeometryNV = ^VkGeometryNV;
     VkGeometryNV = record
       sType :VkStructureType;
       pNext :P_void;
       geometryType :VkGeometryTypeKHR;
       geometry :VkGeometryDataNV;
       flags :VkGeometryFlagsKHR;
     end;

type P_VkAccelerationStructureInfoNV = ^VkAccelerationStructureInfoNV;
     VkAccelerationStructureInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkAccelerationStructureTypeNV;
       flags :VkBuildAccelerationStructureFlagsNV;
       instanceCount :T_uint32_t;
       geometryCount :T_uint32_t;
       pGeometries :P_VkGeometryNV;
     end;

type P_VkAccelerationStructureCreateInfoNV = ^VkAccelerationStructureCreateInfoNV;
     VkAccelerationStructureCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       compactedSize :VkDeviceSize;
       info :VkAccelerationStructureInfoNV;
     end;

type P_VkBindAccelerationStructureMemoryInfoNV = ^VkBindAccelerationStructureMemoryInfoNV;
     VkBindAccelerationStructureMemoryInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       accelerationStructure :VkAccelerationStructureNV;
       memory :VkDeviceMemory;
       memoryOffset :VkDeviceSize;
       deviceIndexCount :T_uint32_t;
       pDeviceIndices :P_uint32_t;
     end;

type P_VkWriteDescriptorSetAccelerationStructureNV = ^VkWriteDescriptorSetAccelerationStructureNV;
     VkWriteDescriptorSetAccelerationStructureNV = record
       sType :VkStructureType;
       pNext :P_void;
       accelerationStructureCount :T_uint32_t;
       pAccelerationStructures :P_VkAccelerationStructureNV;
     end;

type P_VkAccelerationStructureMemoryRequirementsInfoNV = ^VkAccelerationStructureMemoryRequirementsInfoNV;
     VkAccelerationStructureMemoryRequirementsInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkAccelerationStructureMemoryRequirementsTypeNV;
       accelerationStructure :VkAccelerationStructureNV;
     end;

type P_VkPhysicalDeviceRayTracingPropertiesNV = ^VkPhysicalDeviceRayTracingPropertiesNV;
     VkPhysicalDeviceRayTracingPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       shaderGroupHandleSize :T_uint32_t;
       maxRecursionDepth :T_uint32_t;
       maxShaderGroupStride :T_uint32_t;
       shaderGroupBaseAlignment :T_uint32_t;
       maxGeometryCount :T_uint64_t;
       maxInstanceCount :T_uint64_t;
       maxTriangleCount :T_uint64_t;
       maxDescriptorSetAccelerationStructures :T_uint32_t;
     end;

type P_VkTransformMatrixKHR = ^VkTransformMatrixKHR;
     VkTransformMatrixKHR = record
       matrix_:array [ 0..3-1, 0..4-1 ] of T_float;
     end;

type P_VkTransformMatrixNV = ^VkTransformMatrixNV;
     VkTransformMatrixNV = VkTransformMatrixKHR;

type P_VkAabbPositionsKHR = ^VkAabbPositionsKHR;
     VkAabbPositionsKHR = record
       minX :T_float;
       minY :T_float;
       minZ :T_float;
       maxX :T_float;
       maxY :T_float;
       maxZ :T_float;
     end;

type P_VkAabbPositionsNV = ^VkAabbPositionsNV;
     VkAabbPositionsNV = VkAabbPositionsKHR;

type P_VkAccelerationStructureInstanceKHR = ^VkAccelerationStructureInstanceKHR;
     VkAccelerationStructureInstanceKHR = record
       transform :VkTransformMatrixKHR;
       instanceCustomIndex :T_uint32_t; {:24}
       mask :T_uint32_t; {:8}
       instanceShaderBindingTableRecordOffset: T_uint32_t; {:24}
       flags :VkGeometryInstanceFlagsKHR; {:8}
       accelerationStructureReference :T_uint64_t;
     end;

type P_VkAccelerationStructureInstanceNV = ^VkAccelerationStructureInstanceNV;
     VkAccelerationStructureInstanceNV = VkAccelerationStructureInstanceKHR;

type PFN_vkCreateAccelerationStructureNV = function( device_:VkDevice; const pCreateInfo_:P_VkAccelerationStructureCreateInfoNV; const pAllocator_:P_VkAllocationCallbacks; pAccelerationStructure_:P_VkAccelerationStructureNV ) :VkResult;
type PFN_vkDestroyAccelerationStructureNV = procedure( device_:VkDevice; accelerationStructure_:VkAccelerationStructureNV; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkGetAccelerationStructureMemoryRequirementsNV = procedure( device_:VkDevice; const pInfo_:P_VkAccelerationStructureMemoryRequirementsInfoNV; pMemoryRequirements_:P_VkMemoryRequirements2KHR );
type PFN_vkBindAccelerationStructureMemoryNV = function( device_:VkDevice; bindInfoCount_:T_uint32_t; const pBindInfos_:P_VkBindAccelerationStructureMemoryInfoNV ) :VkResult;
type PFN_vkCmdBuildAccelerationStructureNV = procedure( commandBuffer_:VkCommandBuffer; const pInfo_:P_VkAccelerationStructureInfoNV; instanceData_:VkBuffer; instanceOffset_:VkDeviceSize; update_:VkBool32; dst_:VkAccelerationStructureNV; src_:VkAccelerationStructureNV; scratch_:VkBuffer; scratchOffset_:VkDeviceSize );
type PFN_vkCmdCopyAccelerationStructureNV = procedure( commandBuffer_:VkCommandBuffer; dst_:VkAccelerationStructureNV; src_:VkAccelerationStructureNV; mode_:VkCopyAccelerationStructureModeKHR );
type PFN_vkCmdTraceRaysNV = procedure( commandBuffer_:VkCommandBuffer; raygenShaderBindingTableBuffer_:VkBuffer; raygenShaderBindingOffset_:VkDeviceSize; missShaderBindingTableBuffer_:VkBuffer; missShaderBindingOffset_:VkDeviceSize; missShaderBindingStride_:VkDeviceSize; hitShaderBindingTableBuffer_:VkBuffer; hitShaderBindingOffset_:VkDeviceSize; hitShaderBindingStride_:VkDeviceSize; callableShaderBindingTableBuffer_:VkBuffer; callableShaderBindingOffset_:VkDeviceSize; callableShaderBindingStride_:VkDeviceSize; width_:T_uint32_t; height_:T_uint32_t; depth_:T_uint32_t );
type PFN_vkCreateRayTracingPipelinesNV = function( device_:VkDevice; pipelineCache_:VkPipelineCache; createInfoCount_:T_uint32_t; const pCreateInfos_:P_VkRayTracingPipelineCreateInfoNV; const pAllocator_:P_VkAllocationCallbacks; pPipelines_:P_VkPipeline ) :VkResult;
type PFN_vkGetRayTracingShaderGroupHandlesKHR = function( device_:VkDevice; pipeline_:VkPipeline; firstGroup_:T_uint32_t; groupCount_:T_uint32_t; dataSize_:T_size_t; pData_:P_void ) :VkResult;
type PFN_vkGetRayTracingShaderGroupHandlesNV = function( device_:VkDevice; pipeline_:VkPipeline; firstGroup_:T_uint32_t; groupCount_:T_uint32_t; dataSize_:T_size_t; pData_:P_void ) :VkResult;
type PFN_vkGetAccelerationStructureHandleNV = function( device_:VkDevice; accelerationStructure_:VkAccelerationStructureNV; dataSize_:T_size_t; pData_:P_void ) :VkResult;
type PFN_vkCmdWriteAccelerationStructuresPropertiesNV = procedure( commandBuffer_:VkCommandBuffer; accelerationStructureCount_:T_uint32_t; const pAccelerationStructures_:P_VkAccelerationStructureNV; queryType_:VkQueryType; queryPool_:VkQueryPool; firstQuery_:T_uint32_t );
type PFN_vkCompileDeferredNV = function( device_:VkDevice; pipeline_:VkPipeline; shader_:T_uint32_t ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateAccelerationStructureNV(
    device_:VkDevice;
    pCreateInfo_:P_VkAccelerationStructureCreateInfoNV;
    pAllocator_:P_VkAllocationCallbacks;
    pAccelerationStructure_:P_VkAccelerationStructureNV ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyAccelerationStructureNV(
    device_:VkDevice;
    accelerationStructure_:VkAccelerationStructureNV;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkGetAccelerationStructureMemoryRequirementsNV(
    device_:VkDevice;
    pInfo_:P_VkAccelerationStructureMemoryRequirementsInfoNV;
    pMemoryRequirements_:P_VkMemoryRequirements2KHR ); stdcall; external DLLNAME;

function vkBindAccelerationStructureMemoryNV(
    device_:VkDevice;
    bindInfoCount_:T_uint32_t;
    pBindInfos_:P_VkBindAccelerationStructureMemoryInfoNV ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdBuildAccelerationStructureNV(
    commandBuffer_:VkCommandBuffer;
    pInfo_:P_VkAccelerationStructureInfoNV;
    instanceData_:VkBuffer;
    instanceOffset_:VkDeviceSize;
    update_:VkBool32;
    dst_:VkAccelerationStructureNV;
    src_:VkAccelerationStructureNV;
    scratch_:VkBuffer;
    scratchOffset_:VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdCopyAccelerationStructureNV(
    commandBuffer_:VkCommandBuffer;
    dst_:VkAccelerationStructureNV;
    src_:VkAccelerationStructureNV;
    mode_:VkCopyAccelerationStructureModeKHR ); stdcall; external DLLNAME;

procedure vkCmdTraceRaysNV(
    commandBuffer_:VkCommandBuffer;
    raygenShaderBindingTableBuffer_:VkBuffer;
    raygenShaderBindingOffset_:VkDeviceSize;
    missShaderBindingTableBuffer_:VkBuffer;
    missShaderBindingOffset_:VkDeviceSize;
    missShaderBindingStride_:VkDeviceSize;
    hitShaderBindingTableBuffer_:VkBuffer;
    hitShaderBindingOffset_:VkDeviceSize;
    hitShaderBindingStride_:VkDeviceSize;
    callableShaderBindingTableBuffer_:VkBuffer;
    callableShaderBindingOffset_:VkDeviceSize;
    callableShaderBindingStride_:VkDeviceSize;
    width_:T_uint32_t;
    height_:T_uint32_t;
    depth_:T_uint32_t ); stdcall; external DLLNAME;

function vkCreateRayTracingPipelinesNV(
    device_:VkDevice;
    pipelineCache_:VkPipelineCache;
    createInfoCount_:T_uint32_t;
    pCreateInfos_:P_VkRayTracingPipelineCreateInfoNV;
    pAllocator_:P_VkAllocationCallbacks;
    pPipelines_:P_VkPipeline ) :VkResult; stdcall; external DLLNAME;

function vkGetRayTracingShaderGroupHandlesKHR(
    device_:VkDevice;
    pipeline_:VkPipeline;
    firstGroup_:T_uint32_t;
    groupCount_:T_uint32_t;
    dataSize_:T_size_t;
    pData_:P_void ) :VkResult; stdcall; external DLLNAME;

function vkGetRayTracingShaderGroupHandlesNV(
    device_:VkDevice;
    pipeline_:VkPipeline;
    firstGroup_:T_uint32_t;
    groupCount_:T_uint32_t;
    dataSize_:T_size_t;
    pData_:P_void ) :VkResult; stdcall; external DLLNAME;

function vkGetAccelerationStructureHandleNV(
    device_:VkDevice;
    accelerationStructure_:VkAccelerationStructureNV;
    dataSize_:T_size_t;
    pData_:P_void ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdWriteAccelerationStructuresPropertiesNV(
    commandBuffer_:VkCommandBuffer;
    accelerationStructureCount_:T_uint32_t;
    pAccelerationStructures_:P_VkAccelerationStructureNV;
    queryType_:VkQueryType;
    queryPool_:VkQueryPool;
    firstQuery_:T_uint32_t ); stdcall; external DLLNAME;

function vkCompileDeferredNV(
    device_:VkDevice;
    pipeline_:VkPipeline;
    shader_:T_uint32_t ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_representative_fragment_test = 1;
const VK_NV_REPRESENTATIVE_FRAGMENT_TEST_SPEC_VERSION = 2;
const VK_NV_REPRESENTATIVE_FRAGMENT_TEST_EXTENSION_NAME = 'VK_NV_representative_fragment_test';
type P_VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV = ^VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV;
     VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       representativeFragmentTest :VkBool32;
     end;

type P_VkPipelineRepresentativeFragmentTestStateCreateInfoNV = ^VkPipelineRepresentativeFragmentTestStateCreateInfoNV;
     VkPipelineRepresentativeFragmentTestStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       representativeFragmentTestEnable :VkBool32;
     end;



const VK_EXT_filter_cubic = 1;
const VK_EXT_FILTER_CUBIC_SPEC_VERSION  = 3;
const VK_EXT_FILTER_CUBIC_EXTENSION_NAME = 'VK_EXT_filter_cubic';
type P_VkPhysicalDeviceImageViewImageFormatInfoEXT = ^VkPhysicalDeviceImageViewImageFormatInfoEXT;
     VkPhysicalDeviceImageViewImageFormatInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       imageViewType :VkImageViewType;
     end;

type P_VkFilterCubicImageViewImageFormatPropertiesEXT = ^VkFilterCubicImageViewImageFormatPropertiesEXT;
     VkFilterCubicImageViewImageFormatPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       filterCubic :VkBool32;
       filterCubicMinmax :VkBool32;
     end;



const VK_QCOM_render_pass_shader_resolve = 1;
const VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4;
const VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME = 'VK_QCOM_render_pass_shader_resolve';


const VK_EXT_global_priority = 1;
const VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2;
const VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME = 'VK_EXT_global_priority';

type P_VkQueueGlobalPriorityEXT = ^VkQueueGlobalPriorityEXT;
     VkQueueGlobalPriorityEXT = (
       VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT = 128,
       VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = 256,
       VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT = 512,
       VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = 1024,
       VK_QUEUE_GLOBAL_PRIORITY_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkDeviceQueueGlobalPriorityCreateInfoEXT = ^VkDeviceQueueGlobalPriorityCreateInfoEXT;
     VkDeviceQueueGlobalPriorityCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       globalPriority :VkQueueGlobalPriorityEXT;
     end;



const VK_EXT_external_memory_host = 1;
const VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1;
const VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = 'VK_EXT_external_memory_host';
type P_VkImportMemoryHostPointerInfoEXT = ^VkImportMemoryHostPointerInfoEXT;
     VkImportMemoryHostPointerInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       handleType :VkExternalMemoryHandleTypeFlagBits;
       pHostPointer :P_void;
     end;

type P_VkMemoryHostPointerPropertiesEXT = ^VkMemoryHostPointerPropertiesEXT;
     VkMemoryHostPointerPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       memoryTypeBits :T_uint32_t;
     end;

type P_VkPhysicalDeviceExternalMemoryHostPropertiesEXT = ^VkPhysicalDeviceExternalMemoryHostPropertiesEXT;
     VkPhysicalDeviceExternalMemoryHostPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       minImportedHostPointerAlignment :VkDeviceSize;
     end;

type PFN_vkGetMemoryHostPointerPropertiesEXT = function( device_:VkDevice; handleType_:VkExternalMemoryHandleTypeFlagBits; const pHostPointer_:P_void; pMemoryHostPointerProperties_:P_VkMemoryHostPointerPropertiesEXT ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetMemoryHostPointerPropertiesEXT(
    device_:VkDevice;
    handleType_:VkExternalMemoryHandleTypeFlagBits;
    pHostPointer_:P_void;
    pMemoryHostPointerProperties_:P_VkMemoryHostPointerPropertiesEXT ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_buffer_marker = 1;
const VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1;
const VK_AMD_BUFFER_MARKER_EXTENSION_NAME = 'VK_AMD_buffer_marker';
type PFN_vkCmdWriteBufferMarkerAMD = procedure( commandBuffer_:VkCommandBuffer; pipelineStage_:VkPipelineStageFlagBits; dstBuffer_:VkBuffer; dstOffset_:VkDeviceSize; marker_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdWriteBufferMarkerAMD(
    commandBuffer_:VkCommandBuffer;
    pipelineStage_:VkPipelineStageFlagBits;
    dstBuffer_:VkBuffer;
    dstOffset_:VkDeviceSize;
    marker_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_pipeline_compiler_control = 1;
const VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1;
const VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME = 'VK_AMD_pipeline_compiler_control';

type P_VkPipelineCompilerControlFlagBitsAMD = ^VkPipelineCompilerControlFlagBitsAMD;
     VkPipelineCompilerControlFlagBitsAMD = (
       VK_PIPELINE_COMPILER_CONTROL_FLAG_BITS_MAX_ENUM_AMD = $7FFFFFFF
     );
type P_VkPipelineCompilerControlFlagsAMD = ^VkPipelineCompilerControlFlagsAMD;
     VkPipelineCompilerControlFlagsAMD = VkFlags;
type P_VkPipelineCompilerControlCreateInfoAMD = ^VkPipelineCompilerControlCreateInfoAMD;
     VkPipelineCompilerControlCreateInfoAMD = record
       sType :VkStructureType;
       pNext :P_void;
       compilerControlFlags :VkPipelineCompilerControlFlagsAMD;
     end;



const VK_EXT_calibrated_timestamps = 1;
const VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1;
const VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = 'VK_EXT_calibrated_timestamps';

type P_VkTimeDomainEXT = ^VkTimeDomainEXT;
     VkTimeDomainEXT = (
       VK_TIME_DOMAIN_DEVICE_EXT = 0,
       VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT = 1,
       VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = 2,
       VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = 3,
       VK_TIME_DOMAIN_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkCalibratedTimestampInfoEXT = ^VkCalibratedTimestampInfoEXT;
     VkCalibratedTimestampInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       timeDomain :VkTimeDomainEXT;
     end;

type PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT = function( physicalDevice_:VkPhysicalDevice; pTimeDomainCount_:P_uint32_t; pTimeDomains_:P_VkTimeDomainEXT ) :VkResult;
type PFN_vkGetCalibratedTimestampsEXT = function( device_:VkDevice; timestampCount_:T_uint32_t; const pTimestampInfos_:P_VkCalibratedTimestampInfoEXT; pTimestamps_:P_uint64_t; pMaxDeviation_:P_uint64_t ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceCalibrateableTimeDomainsEXT(
    physicalDevice_:VkPhysicalDevice;
    pTimeDomainCount_:P_uint32_t;
    pTimeDomains_:P_VkTimeDomainEXT ) :VkResult; stdcall; external DLLNAME;

function vkGetCalibratedTimestampsEXT(
    device_:VkDevice;
    timestampCount_:T_uint32_t;
    pTimestampInfos_:P_VkCalibratedTimestampInfoEXT;
    pTimestamps_:P_uint64_t;
    pMaxDeviation_:P_uint64_t ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_AMD_shader_core_properties = 1;
const VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 2;
const VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = 'VK_AMD_shader_core_properties';
type P_VkPhysicalDeviceShaderCorePropertiesAMD = ^VkPhysicalDeviceShaderCorePropertiesAMD;
     VkPhysicalDeviceShaderCorePropertiesAMD = record
       sType :VkStructureType;
       pNext :P_void;
       shaderEngineCount :T_uint32_t;
       shaderArraysPerEngineCount :T_uint32_t;
       computeUnitsPerShaderArray :T_uint32_t;
       simdPerComputeUnit :T_uint32_t;
       wavefrontsPerSimd :T_uint32_t;
       wavefrontSize :T_uint32_t;
       sgprsPerSimd :T_uint32_t;
       minSgprAllocation :T_uint32_t;
       maxSgprAllocation :T_uint32_t;
       sgprAllocationGranularity :T_uint32_t;
       vgprsPerSimd :T_uint32_t;
       minVgprAllocation :T_uint32_t;
       maxVgprAllocation :T_uint32_t;
       vgprAllocationGranularity :T_uint32_t;
     end;



const VK_AMD_memory_overallocation_behavior = 1;
const VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1;
const VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = 'VK_AMD_memory_overallocation_behavior';

type P_VkMemoryOverallocationBehaviorAMD = ^VkMemoryOverallocationBehaviorAMD;
     VkMemoryOverallocationBehaviorAMD = (
       VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD = 0,
       VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD = 1,
       VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD = 2,
       VK_MEMORY_OVERALLOCATION_BEHAVIOR_MAX_ENUM_AMD = $7FFFFFFF
     );
type P_VkDeviceMemoryOverallocationCreateInfoAMD = ^VkDeviceMemoryOverallocationCreateInfoAMD;
     VkDeviceMemoryOverallocationCreateInfoAMD = record
       sType :VkStructureType;
       pNext :P_void;
       overallocationBehavior :VkMemoryOverallocationBehaviorAMD;
     end;



const VK_EXT_vertex_attribute_divisor = 1;
const VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 3;
const VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = 'VK_EXT_vertex_attribute_divisor';
type P_VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT = ^VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT;
     VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxVertexAttribDivisor :T_uint32_t;
     end;

type P_VkVertexInputBindingDivisorDescriptionEXT = ^VkVertexInputBindingDivisorDescriptionEXT;
     VkVertexInputBindingDivisorDescriptionEXT = record
       binding :T_uint32_t;
       divisor :T_uint32_t;
     end;

type P_VkPipelineVertexInputDivisorStateCreateInfoEXT = ^VkPipelineVertexInputDivisorStateCreateInfoEXT;
     VkPipelineVertexInputDivisorStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       vertexBindingDivisorCount :T_uint32_t;
       pVertexBindingDivisors :P_VkVertexInputBindingDivisorDescriptionEXT;
     end;

type P_VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT = ^VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT;
     VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       vertexAttributeInstanceRateDivisor :VkBool32;
       vertexAttributeInstanceRateZeroDivisor :VkBool32;
     end;



const VK_EXT_pipeline_creation_feedback = 1;
const VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1;
const VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = 'VK_EXT_pipeline_creation_feedback';

type P_VkPipelineCreationFeedbackFlagBitsEXT = ^VkPipelineCreationFeedbackFlagBitsEXT;
     VkPipelineCreationFeedbackFlagBitsEXT = (
       VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = $00000001,
       VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = $00000002,
       VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = $00000004,
       VK_PIPELINE_CREATION_FEEDBACK_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkPipelineCreationFeedbackFlagsEXT = ^VkPipelineCreationFeedbackFlagsEXT;
     VkPipelineCreationFeedbackFlagsEXT = VkFlags;
type P_VkPipelineCreationFeedbackEXT = ^VkPipelineCreationFeedbackEXT;
     VkPipelineCreationFeedbackEXT = record
       flags :VkPipelineCreationFeedbackFlagsEXT;
       duration :T_uint64_t;
     end;

type P_VkPipelineCreationFeedbackCreateInfoEXT = ^VkPipelineCreationFeedbackCreateInfoEXT;
     VkPipelineCreationFeedbackCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       pPipelineCreationFeedback :P_VkPipelineCreationFeedbackEXT;
       pipelineStageCreationFeedbackCount :T_uint32_t;
       pPipelineStageCreationFeedbacks :P_VkPipelineCreationFeedbackEXT;
     end;



const VK_NV_shader_subgroup_partitioned = 1;
const VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1;
const VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = 'VK_NV_shader_subgroup_partitioned';


const VK_NV_compute_shader_derivatives = 1;
const VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1;
const VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = 'VK_NV_compute_shader_derivatives';
type P_VkPhysicalDeviceComputeShaderDerivativesFeaturesNV = ^VkPhysicalDeviceComputeShaderDerivativesFeaturesNV;
     VkPhysicalDeviceComputeShaderDerivativesFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       computeDerivativeGroupQuads :VkBool32;
       computeDerivativeGroupLinear :VkBool32;
     end;



const VK_NV_mesh_shader = 1;
const VK_NV_MESH_SHADER_SPEC_VERSION    = 1;
const VK_NV_MESH_SHADER_EXTENSION_NAME = 'VK_NV_mesh_shader';
type P_VkPhysicalDeviceMeshShaderFeaturesNV = ^VkPhysicalDeviceMeshShaderFeaturesNV;
     VkPhysicalDeviceMeshShaderFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       taskShader :VkBool32;
       meshShader :VkBool32;
     end;

type P_VkPhysicalDeviceMeshShaderPropertiesNV = ^VkPhysicalDeviceMeshShaderPropertiesNV;
     VkPhysicalDeviceMeshShaderPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       maxDrawMeshTasksCount :T_uint32_t;
       maxTaskWorkGroupInvocations :T_uint32_t;
       maxTaskWorkGroupSize :array [ 0..3-1 ] of T_uint32_t;
       maxTaskTotalMemorySize :T_uint32_t;
       maxTaskOutputCount :T_uint32_t;
       maxMeshWorkGroupInvocations :T_uint32_t;
       maxMeshWorkGroupSize :array [ 0..3-1 ] of T_uint32_t;
       maxMeshTotalMemorySize :T_uint32_t;
       maxMeshOutputVertices :T_uint32_t;
       maxMeshOutputPrimitives :T_uint32_t;
       maxMeshMultiviewViewCount :T_uint32_t;
       meshOutputPerVertexGranularity :T_uint32_t;
       meshOutputPerPrimitiveGranularity :T_uint32_t;
     end;

type P_VkDrawMeshTasksIndirectCommandNV = ^VkDrawMeshTasksIndirectCommandNV;
     VkDrawMeshTasksIndirectCommandNV = record
       taskCount :T_uint32_t;
       firstTask :T_uint32_t;
     end;

type PFN_vkCmdDrawMeshTasksNV = procedure( commandBuffer_:VkCommandBuffer; taskCount_:T_uint32_t; firstTask_:T_uint32_t );
type PFN_vkCmdDrawMeshTasksIndirectNV = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; drawCount_:T_uint32_t; stride_:T_uint32_t );
type PFN_vkCmdDrawMeshTasksIndirectCountNV = procedure( commandBuffer_:VkCommandBuffer; buffer_:VkBuffer; offset_:VkDeviceSize; countBuffer_:VkBuffer; countBufferOffset_:VkDeviceSize; maxDrawCount_:T_uint32_t; stride_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdDrawMeshTasksNV(
    commandBuffer_:VkCommandBuffer;
    taskCount_:T_uint32_t;
    firstTask_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawMeshTasksIndirectNV(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    drawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkCmdDrawMeshTasksIndirectCountNV(
    commandBuffer_:VkCommandBuffer;
    buffer_:VkBuffer;
    offset_:VkDeviceSize;
    countBuffer_:VkBuffer;
    countBufferOffset_:VkDeviceSize;
    maxDrawCount_:T_uint32_t;
    stride_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_fragment_shader_barycentric = 1;
const VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1;
const VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = 'VK_NV_fragment_shader_barycentric';
type P_VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV = ^VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV;
     VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentShaderBarycentric :VkBool32;
     end;



const VK_NV_shader_image_footprint = 1;
const VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 2;
const VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = 'VK_NV_shader_image_footprint';
type P_VkPhysicalDeviceShaderImageFootprintFeaturesNV = ^VkPhysicalDeviceShaderImageFootprintFeaturesNV;
     VkPhysicalDeviceShaderImageFootprintFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       imageFootprint :VkBool32;
     end;



const VK_NV_scissor_exclusive = 1;
const VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1;
const VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = 'VK_NV_scissor_exclusive';
type P_VkPipelineViewportExclusiveScissorStateCreateInfoNV = ^VkPipelineViewportExclusiveScissorStateCreateInfoNV;
     VkPipelineViewportExclusiveScissorStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       exclusiveScissorCount :T_uint32_t;
       pExclusiveScissors :P_VkRect2D;
     end;

type P_VkPhysicalDeviceExclusiveScissorFeaturesNV = ^VkPhysicalDeviceExclusiveScissorFeaturesNV;
     VkPhysicalDeviceExclusiveScissorFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       exclusiveScissor :VkBool32;
     end;

type PFN_vkCmdSetExclusiveScissorNV = procedure( commandBuffer_:VkCommandBuffer; firstExclusiveScissor_:T_uint32_t; exclusiveScissorCount_:T_uint32_t; const pExclusiveScissors_:P_VkRect2D );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetExclusiveScissorNV(
    commandBuffer_:VkCommandBuffer;
    firstExclusiveScissor_:T_uint32_t;
    exclusiveScissorCount_:T_uint32_t;
    pExclusiveScissors_:P_VkRect2D ); stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_device_diagnostic_checkpoints = 1;
const VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = 2;
const VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME = 'VK_NV_device_diagnostic_checkpoints';
type P_VkQueueFamilyCheckpointPropertiesNV = ^VkQueueFamilyCheckpointPropertiesNV;
     VkQueueFamilyCheckpointPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       checkpointExecutionStageMask :VkPipelineStageFlags;
     end;

type P_VkCheckpointDataNV = ^VkCheckpointDataNV;
     VkCheckpointDataNV = record
       sType :VkStructureType;
       pNext :P_void;
       stage :VkPipelineStageFlagBits;
       pCheckpointMarker :P_void;
     end;

type PFN_vkCmdSetCheckpointNV = procedure( commandBuffer_:VkCommandBuffer; const pCheckpointMarker_:P_void );
type PFN_vkGetQueueCheckpointDataNV = procedure( queue_:VkQueue; pCheckpointDataCount_:P_uint32_t; pCheckpointData_:P_VkCheckpointDataNV );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetCheckpointNV(
    commandBuffer_:VkCommandBuffer;
    pCheckpointMarker_:P_void ); stdcall; external DLLNAME;

procedure vkGetQueueCheckpointDataNV(
    queue_:VkQueue;
    pCheckpointDataCount_:P_uint32_t;
    pCheckpointData_:P_VkCheckpointDataNV ); stdcall; external DLLNAME;
{$ENDIF}


const VK_INTEL_shader_integer_functions2 = 1;
const VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION = 1;
const VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME = 'VK_INTEL_shader_integer_functions2';
type P_VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = ^VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL;
     VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       shaderIntegerFunctions2 :VkBool32;
     end;



const VK_INTEL_performance_query = 1;
type P_VkPerformanceConfigurationINTEL = ^VkPerformanceConfigurationINTEL;
     VkPerformanceConfigurationINTEL = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION = 2;
const VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME = 'VK_INTEL_performance_query';

type P_VkPerformanceConfigurationTypeINTEL = ^VkPerformanceConfigurationTypeINTEL;
     VkPerformanceConfigurationTypeINTEL = (
       VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL = 0,
       VK_PERFORMANCE_CONFIGURATION_TYPE_MAX_ENUM_INTEL = $7FFFFFFF
     );

type P_VkQueryPoolSamplingModeINTEL = ^VkQueryPoolSamplingModeINTEL;
     VkQueryPoolSamplingModeINTEL = (
       VK_QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL = 0,
       VK_QUERY_POOL_SAMPLING_MODE_MAX_ENUM_INTEL = $7FFFFFFF
     );

type P_VkPerformanceOverrideTypeINTEL = ^VkPerformanceOverrideTypeINTEL;
     VkPerformanceOverrideTypeINTEL = (
       VK_PERFORMANCE_OVERRIDE_TYPE_NULL_HARDWARE_INTEL = 0,
       VK_PERFORMANCE_OVERRIDE_TYPE_FLUSH_GPU_CACHES_INTEL = 1,
       VK_PERFORMANCE_OVERRIDE_TYPE_MAX_ENUM_INTEL = $7FFFFFFF
     );

type P_VkPerformanceParameterTypeINTEL = ^VkPerformanceParameterTypeINTEL;
     VkPerformanceParameterTypeINTEL = (
       VK_PERFORMANCE_PARAMETER_TYPE_HW_COUNTERS_SUPPORTED_INTEL = 0,
       VK_PERFORMANCE_PARAMETER_TYPE_STREAM_MARKER_VALID_BITS_INTEL = 1,
       VK_PERFORMANCE_PARAMETER_TYPE_MAX_ENUM_INTEL = $7FFFFFFF
     );

type P_VkPerformanceValueTypeINTEL = ^VkPerformanceValueTypeINTEL;
     VkPerformanceValueTypeINTEL = (
       VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL = 0,
       VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL = 1,
       VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL = 2,
       VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL = 3,
       VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL = 4,
       VK_PERFORMANCE_VALUE_TYPE_MAX_ENUM_INTEL = $7FFFFFFF
     );
type P_VkPerformanceValueDataINTEL = ^VkPerformanceValueDataINTEL;
     VkPerformanceValueDataINTEL = record
     case Byte of
       0:( value32     :T_uint32_t );
       1:( value64     :T_uint64_t );
       2:( valueFloat  :T_float    );
       3:( valueBool   :VkBool32 );
       4:( valueString :P_char   );
     end;

type P_VkPerformanceValueINTEL = ^VkPerformanceValueINTEL;
     VkPerformanceValueINTEL = record
       type_ :VkPerformanceValueTypeINTEL;
       data :VkPerformanceValueDataINTEL;
     end;

type P_VkInitializePerformanceApiInfoINTEL = ^VkInitializePerformanceApiInfoINTEL;
     VkInitializePerformanceApiInfoINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       pUserData :P_void;
     end;

type P_VkQueryPoolPerformanceQueryCreateInfoINTEL = ^VkQueryPoolPerformanceQueryCreateInfoINTEL;
     VkQueryPoolPerformanceQueryCreateInfoINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       performanceCountersSampling :VkQueryPoolSamplingModeINTEL;
     end;

type P_VkQueryPoolCreateInfoINTEL = ^VkQueryPoolCreateInfoINTEL;
     VkQueryPoolCreateInfoINTEL = VkQueryPoolPerformanceQueryCreateInfoINTEL;

type P_VkPerformanceMarkerInfoINTEL = ^VkPerformanceMarkerInfoINTEL;
     VkPerformanceMarkerInfoINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       marker :T_uint64_t;
     end;

type P_VkPerformanceStreamMarkerInfoINTEL = ^VkPerformanceStreamMarkerInfoINTEL;
     VkPerformanceStreamMarkerInfoINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       marker :T_uint32_t;
     end;

type P_VkPerformanceOverrideInfoINTEL = ^VkPerformanceOverrideInfoINTEL;
     VkPerformanceOverrideInfoINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkPerformanceOverrideTypeINTEL;
       enable :VkBool32;
       parameter :T_uint64_t;
     end;

type P_VkPerformanceConfigurationAcquireInfoINTEL = ^VkPerformanceConfigurationAcquireInfoINTEL;
     VkPerformanceConfigurationAcquireInfoINTEL = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkPerformanceConfigurationTypeINTEL;
     end;

type PFN_vkInitializePerformanceApiINTEL = function( device_:VkDevice; const pInitializeInfo_:P_VkInitializePerformanceApiInfoINTEL ) :VkResult;
type PFN_vkUninitializePerformanceApiINTEL = procedure( device_:VkDevice );
type PFN_vkCmdSetPerformanceMarkerINTEL = function( commandBuffer_:VkCommandBuffer; const pMarkerInfo_:P_VkPerformanceMarkerInfoINTEL ) :VkResult;
type PFN_vkCmdSetPerformanceStreamMarkerINTEL = function( commandBuffer_:VkCommandBuffer; const pMarkerInfo_:P_VkPerformanceStreamMarkerInfoINTEL ) :VkResult;
type PFN_vkCmdSetPerformanceOverrideINTEL = function( commandBuffer_:VkCommandBuffer; const pOverrideInfo_:P_VkPerformanceOverrideInfoINTEL ) :VkResult;
type PFN_vkAcquirePerformanceConfigurationINTEL = function( device_:VkDevice; const pAcquireInfo_:P_VkPerformanceConfigurationAcquireInfoINTEL; pConfiguration_:P_VkPerformanceConfigurationINTEL ) :VkResult;
type PFN_vkReleasePerformanceConfigurationINTEL = function( device_:VkDevice; configuration_:VkPerformanceConfigurationINTEL ) :VkResult;
type PFN_vkQueueSetPerformanceConfigurationINTEL = function( queue_:VkQueue; configuration_:VkPerformanceConfigurationINTEL ) :VkResult;
type PFN_vkGetPerformanceParameterINTEL = function( device_:VkDevice; parameter_:VkPerformanceParameterTypeINTEL; pValue_:P_VkPerformanceValueINTEL ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkInitializePerformanceApiINTEL(
    device_:VkDevice;
    pInitializeInfo_:P_VkInitializePerformanceApiInfoINTEL ) :VkResult; stdcall; external DLLNAME;

procedure vkUninitializePerformanceApiINTEL(
    device_:VkDevice ); stdcall; external DLLNAME;

function vkCmdSetPerformanceMarkerINTEL(
    commandBuffer_:VkCommandBuffer;
    pMarkerInfo_:P_VkPerformanceMarkerInfoINTEL ) :VkResult; stdcall; external DLLNAME;

function vkCmdSetPerformanceStreamMarkerINTEL(
    commandBuffer_:VkCommandBuffer;
    pMarkerInfo_:P_VkPerformanceStreamMarkerInfoINTEL ) :VkResult; stdcall; external DLLNAME;

function vkCmdSetPerformanceOverrideINTEL(
    commandBuffer_:VkCommandBuffer;
    pOverrideInfo_:P_VkPerformanceOverrideInfoINTEL ) :VkResult; stdcall; external DLLNAME;

function vkAcquirePerformanceConfigurationINTEL(
    device_:VkDevice;
    pAcquireInfo_:P_VkPerformanceConfigurationAcquireInfoINTEL;
    pConfiguration_:P_VkPerformanceConfigurationINTEL ) :VkResult; stdcall; external DLLNAME;

function vkReleasePerformanceConfigurationINTEL(
    device_:VkDevice;
    configuration_:VkPerformanceConfigurationINTEL ) :VkResult; stdcall; external DLLNAME;

function vkQueueSetPerformanceConfigurationINTEL(
    queue_:VkQueue;
    configuration_:VkPerformanceConfigurationINTEL ) :VkResult; stdcall; external DLLNAME;

function vkGetPerformanceParameterINTEL(
    device_:VkDevice;
    parameter_:VkPerformanceParameterTypeINTEL;
    pValue_:P_VkPerformanceValueINTEL ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_pci_bus_info = 1;
const VK_EXT_PCI_BUS_INFO_SPEC_VERSION  = 2;
const VK_EXT_PCI_BUS_INFO_EXTENSION_NAME = 'VK_EXT_pci_bus_info';
type P_VkPhysicalDevicePCIBusInfoPropertiesEXT = ^VkPhysicalDevicePCIBusInfoPropertiesEXT;
     VkPhysicalDevicePCIBusInfoPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       pciDomain :T_uint32_t;
       pciBus :T_uint32_t;
       pciDevice :T_uint32_t;
       pciFunction :T_uint32_t;
     end;



const VK_AMD_display_native_hdr = 1;
const VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = 1;
const VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME = 'VK_AMD_display_native_hdr';
type P_VkDisplayNativeHdrSurfaceCapabilitiesAMD = ^VkDisplayNativeHdrSurfaceCapabilitiesAMD;
     VkDisplayNativeHdrSurfaceCapabilitiesAMD = record
       sType :VkStructureType;
       pNext :P_void;
       localDimmingSupport :VkBool32;
     end;

type P_VkSwapchainDisplayNativeHdrCreateInfoAMD = ^VkSwapchainDisplayNativeHdrCreateInfoAMD;
     VkSwapchainDisplayNativeHdrCreateInfoAMD = record
       sType :VkStructureType;
       pNext :P_void;
       localDimmingEnable :VkBool32;
     end;

type PFN_vkSetLocalDimmingAMD = procedure( device_:VkDevice; swapChain_:VkSwapchainKHR; localDimmingEnable_:VkBool32 );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkSetLocalDimmingAMD(
    device_:VkDevice;
    swapChain_:VkSwapchainKHR;
    localDimmingEnable_:VkBool32 ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_fragment_density_map = 1;
const VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1;
const VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME = 'VK_EXT_fragment_density_map';
type P_VkPhysicalDeviceFragmentDensityMapFeaturesEXT = ^VkPhysicalDeviceFragmentDensityMapFeaturesEXT;
     VkPhysicalDeviceFragmentDensityMapFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentDensityMap :VkBool32;
       fragmentDensityMapDynamic :VkBool32;
       fragmentDensityMapNonSubsampledImages :VkBool32;
     end;

type P_VkPhysicalDeviceFragmentDensityMapPropertiesEXT = ^VkPhysicalDeviceFragmentDensityMapPropertiesEXT;
     VkPhysicalDeviceFragmentDensityMapPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       minFragmentDensityTexelSize :VkExtent2D;
       maxFragmentDensityTexelSize :VkExtent2D;
       fragmentDensityInvocations :VkBool32;
     end;

type P_VkRenderPassFragmentDensityMapCreateInfoEXT = ^VkRenderPassFragmentDensityMapCreateInfoEXT;
     VkRenderPassFragmentDensityMapCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentDensityMapAttachment :VkAttachmentReference;
     end;



const VK_EXT_scalar_block_layout = 1;
const VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1;
const VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = 'VK_EXT_scalar_block_layout';
type P_VkPhysicalDeviceScalarBlockLayoutFeaturesEXT = ^VkPhysicalDeviceScalarBlockLayoutFeaturesEXT;
     VkPhysicalDeviceScalarBlockLayoutFeaturesEXT = VkPhysicalDeviceScalarBlockLayoutFeatures;



const VK_GOOGLE_hlsl_functionality1 = 1;
const VK_GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION = 1;
const VK_GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME = 'VK_GOOGLE_hlsl_functionality1';


const VK_GOOGLE_decorate_string = 1;
const VK_GOOGLE_DECORATE_STRING_SPEC_VERSION = 1;
const VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME = 'VK_GOOGLE_decorate_string';


const VK_EXT_subgroup_size_control = 1;
const VK_EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION = 2;
const VK_EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME = 'VK_EXT_subgroup_size_control';
type P_VkPhysicalDeviceSubgroupSizeControlFeaturesEXT = ^VkPhysicalDeviceSubgroupSizeControlFeaturesEXT;
     VkPhysicalDeviceSubgroupSizeControlFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       subgroupSizeControl :VkBool32;
       computeFullSubgroups :VkBool32;
     end;

type P_VkPhysicalDeviceSubgroupSizeControlPropertiesEXT = ^VkPhysicalDeviceSubgroupSizeControlPropertiesEXT;
     VkPhysicalDeviceSubgroupSizeControlPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       minSubgroupSize :T_uint32_t;
       maxSubgroupSize :T_uint32_t;
       maxComputeWorkgroupSubgroups :T_uint32_t;
       requiredSubgroupSizeStages :VkShaderStageFlags;
     end;

type P_VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = ^VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT;
     VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       requiredSubgroupSize :T_uint32_t;
     end;



const VK_AMD_shader_core_properties2 = 1;
const VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1;
const VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME = 'VK_AMD_shader_core_properties2';

type P_VkShaderCorePropertiesFlagBitsAMD = ^VkShaderCorePropertiesFlagBitsAMD;
     VkShaderCorePropertiesFlagBitsAMD = (
       VK_SHADER_CORE_PROPERTIES_FLAG_BITS_MAX_ENUM_AMD = $7FFFFFFF
     );
type P_VkShaderCorePropertiesFlagsAMD = ^VkShaderCorePropertiesFlagsAMD;
     VkShaderCorePropertiesFlagsAMD = VkFlags;
type P_VkPhysicalDeviceShaderCoreProperties2AMD = ^VkPhysicalDeviceShaderCoreProperties2AMD;
     VkPhysicalDeviceShaderCoreProperties2AMD = record
       sType :VkStructureType;
       pNext :P_void;
       shaderCoreFeatures :VkShaderCorePropertiesFlagsAMD;
       activeComputeUnitCount :T_uint32_t;
     end;



const VK_AMD_device_coherent_memory = 1;
const VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1;
const VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME = 'VK_AMD_device_coherent_memory';
type P_VkPhysicalDeviceCoherentMemoryFeaturesAMD = ^VkPhysicalDeviceCoherentMemoryFeaturesAMD;
     VkPhysicalDeviceCoherentMemoryFeaturesAMD = record
       sType :VkStructureType;
       pNext :P_void;
       deviceCoherentMemory :VkBool32;
     end;



const VK_EXT_shader_image_atomic_int64 = 1;
const VK_EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION = 1;
const VK_EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME = 'VK_EXT_shader_image_atomic_int64';
type P_VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT = ^VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT;
     VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       shaderImageInt64Atomics :VkBool32;
       sparseImageInt64Atomics :VkBool32;
     end;



const VK_EXT_memory_budget = 1;
const VK_EXT_MEMORY_BUDGET_SPEC_VERSION = 1;
const VK_EXT_MEMORY_BUDGET_EXTENSION_NAME = 'VK_EXT_memory_budget';
type P_VkPhysicalDeviceMemoryBudgetPropertiesEXT = ^VkPhysicalDeviceMemoryBudgetPropertiesEXT;
     VkPhysicalDeviceMemoryBudgetPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       heapBudget :array [ 0..VK_MAX_MEMORY_HEAPS-1 ] of VkDeviceSize;
       heapUsage :array [ 0..VK_MAX_MEMORY_HEAPS-1 ] of VkDeviceSize;
     end;



const VK_EXT_memory_priority = 1;
const VK_EXT_MEMORY_PRIORITY_SPEC_VERSION = 1;
const VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME = 'VK_EXT_memory_priority';
type P_VkPhysicalDeviceMemoryPriorityFeaturesEXT = ^VkPhysicalDeviceMemoryPriorityFeaturesEXT;
     VkPhysicalDeviceMemoryPriorityFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       memoryPriority :VkBool32;
     end;

type P_VkMemoryPriorityAllocateInfoEXT = ^VkMemoryPriorityAllocateInfoEXT;
     VkMemoryPriorityAllocateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       priority :T_float;
     end;



const VK_NV_dedicated_allocation_image_aliasing = 1;
const VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION = 1;
const VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME = 'VK_NV_dedicated_allocation_image_aliasing';
type P_VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = ^VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV;
     VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       dedicatedAllocationImageAliasing :VkBool32;
     end;



const VK_EXT_buffer_device_address = 1;
const VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2;
const VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = 'VK_EXT_buffer_device_address';
type P_VkPhysicalDeviceBufferDeviceAddressFeaturesEXT = ^VkPhysicalDeviceBufferDeviceAddressFeaturesEXT;
     VkPhysicalDeviceBufferDeviceAddressFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       bufferDeviceAddress :VkBool32;
       bufferDeviceAddressCaptureReplay :VkBool32;
       bufferDeviceAddressMultiDevice :VkBool32;
     end;

type P_VkPhysicalDeviceBufferAddressFeaturesEXT = ^VkPhysicalDeviceBufferAddressFeaturesEXT;
     VkPhysicalDeviceBufferAddressFeaturesEXT = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT;

type P_VkBufferDeviceAddressInfoEXT = ^VkBufferDeviceAddressInfoEXT;
     VkBufferDeviceAddressInfoEXT = VkBufferDeviceAddressInfo;

type P_VkBufferDeviceAddressCreateInfoEXT = ^VkBufferDeviceAddressCreateInfoEXT;
     VkBufferDeviceAddressCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       deviceAddress :VkDeviceAddress;
     end;

type PFN_vkGetBufferDeviceAddressEXT = function( device_:VkDevice; const pInfo_:P_VkBufferDeviceAddressInfo ) :VkDeviceAddress;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetBufferDeviceAddressEXT(
    device_:VkDevice;
    pInfo_:P_VkBufferDeviceAddressInfo ) :VkDeviceAddress; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_tooling_info = 1;
const VK_EXT_TOOLING_INFO_SPEC_VERSION  = 1;
const VK_EXT_TOOLING_INFO_EXTENSION_NAME = 'VK_EXT_tooling_info';

type P_VkToolPurposeFlagBitsEXT = ^VkToolPurposeFlagBitsEXT;
     VkToolPurposeFlagBitsEXT = (
       VK_TOOL_PURPOSE_VALIDATION_BIT_EXT = $00000001,
       VK_TOOL_PURPOSE_PROFILING_BIT_EXT = $00000002,
       VK_TOOL_PURPOSE_TRACING_BIT_EXT = $00000004,
       VK_TOOL_PURPOSE_ADDITIONAL_FEATURES_BIT_EXT = $00000008,
       VK_TOOL_PURPOSE_MODIFYING_FEATURES_BIT_EXT = $00000010,
       VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT = $00000020,
       VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT = $00000040,
       VK_TOOL_PURPOSE_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkToolPurposeFlagsEXT = ^VkToolPurposeFlagsEXT;
     VkToolPurposeFlagsEXT = VkFlags;
type P_VkPhysicalDeviceToolPropertiesEXT = ^VkPhysicalDeviceToolPropertiesEXT;
     VkPhysicalDeviceToolPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       name :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
       version :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
       purposes :VkToolPurposeFlagsEXT;
       description :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
       layer :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
     end;

type PFN_vkGetPhysicalDeviceToolPropertiesEXT = function( physicalDevice_:VkPhysicalDevice; pToolCount_:P_uint32_t; pToolProperties_:P_VkPhysicalDeviceToolPropertiesEXT ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceToolPropertiesEXT(
    physicalDevice_:VkPhysicalDevice;
    pToolCount_:P_uint32_t;
    pToolProperties_:P_VkPhysicalDeviceToolPropertiesEXT ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_separate_stencil_usage = 1;
const VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1;
const VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = 'VK_EXT_separate_stencil_usage';
type P_VkImageStencilUsageCreateInfoEXT = ^VkImageStencilUsageCreateInfoEXT;
     VkImageStencilUsageCreateInfoEXT = VkImageStencilUsageCreateInfo;



const VK_EXT_validation_features = 1;
const VK_EXT_VALIDATION_FEATURES_SPEC_VERSION = 4;
const VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME = 'VK_EXT_validation_features';

type P_VkValidationFeatureEnableEXT = ^VkValidationFeatureEnableEXT;
     VkValidationFeatureEnableEXT = (
       VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT = 0,
       VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = 1,
       VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT = 2,
       VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT = 3,
       VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT = 4,
       VK_VALIDATION_FEATURE_ENABLE_MAX_ENUM_EXT = $7FFFFFFF
     );

type P_VkValidationFeatureDisableEXT = ^VkValidationFeatureDisableEXT;
     VkValidationFeatureDisableEXT = (
       VK_VALIDATION_FEATURE_DISABLE_ALL_EXT = 0,
       VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT = 1,
       VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT = 2,
       VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT = 3,
       VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = 4,
       VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT = 5,
       VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT = 6,
       VK_VALIDATION_FEATURE_DISABLE_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkValidationFeaturesEXT = ^VkValidationFeaturesEXT;
     VkValidationFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       enabledValidationFeatureCount :T_uint32_t;
       pEnabledValidationFeatures :P_VkValidationFeatureEnableEXT;
       disabledValidationFeatureCount :T_uint32_t;
       pDisabledValidationFeatures :P_VkValidationFeatureDisableEXT;
     end;



const VK_NV_cooperative_matrix = 1;
const VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1;
const VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME = 'VK_NV_cooperative_matrix';

type P_VkComponentTypeNV = ^VkComponentTypeNV;
     VkComponentTypeNV = (
       VK_COMPONENT_TYPE_FLOAT16_NV = 0,
       VK_COMPONENT_TYPE_FLOAT32_NV = 1,
       VK_COMPONENT_TYPE_FLOAT64_NV = 2,
       VK_COMPONENT_TYPE_SINT8_NV = 3,
       VK_COMPONENT_TYPE_SINT16_NV = 4,
       VK_COMPONENT_TYPE_SINT32_NV = 5,
       VK_COMPONENT_TYPE_SINT64_NV = 6,
       VK_COMPONENT_TYPE_UINT8_NV = 7,
       VK_COMPONENT_TYPE_UINT16_NV = 8,
       VK_COMPONENT_TYPE_UINT32_NV = 9,
       VK_COMPONENT_TYPE_UINT64_NV = 10,
       VK_COMPONENT_TYPE_MAX_ENUM_NV = $7FFFFFFF
     );

type P_VkScopeNV = ^VkScopeNV;
     VkScopeNV = (
       VK_SCOPE_DEVICE_NV = 1,
       VK_SCOPE_WORKGROUP_NV = 2,
       VK_SCOPE_SUBGROUP_NV = 3,
       VK_SCOPE_QUEUE_FAMILY_NV = 5,
       VK_SCOPE_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkCooperativeMatrixPropertiesNV = ^VkCooperativeMatrixPropertiesNV;
     VkCooperativeMatrixPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       MSize :T_uint32_t;
       NSize :T_uint32_t;
       KSize :T_uint32_t;
       AType :VkComponentTypeNV;
       BType :VkComponentTypeNV;
       CType :VkComponentTypeNV;
       DType :VkComponentTypeNV;
       scope :VkScopeNV;
     end;

type P_VkPhysicalDeviceCooperativeMatrixFeaturesNV = ^VkPhysicalDeviceCooperativeMatrixFeaturesNV;
     VkPhysicalDeviceCooperativeMatrixFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       cooperativeMatrix :VkBool32;
       cooperativeMatrixRobustBufferAccess :VkBool32;
     end;

type P_VkPhysicalDeviceCooperativeMatrixPropertiesNV = ^VkPhysicalDeviceCooperativeMatrixPropertiesNV;
     VkPhysicalDeviceCooperativeMatrixPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       cooperativeMatrixSupportedStages :VkShaderStageFlags;
     end;

type PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV = function( physicalDevice_:VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkCooperativeMatrixPropertiesNV ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceCooperativeMatrixPropertiesNV(
    physicalDevice_:VkPhysicalDevice;
    pPropertyCount_:P_uint32_t;
    pProperties_:P_VkCooperativeMatrixPropertiesNV ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_coverage_reduction_mode = 1;
const VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1;
const VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME = 'VK_NV_coverage_reduction_mode';

type P_VkCoverageReductionModeNV = ^VkCoverageReductionModeNV;
     VkCoverageReductionModeNV = (
       VK_COVERAGE_REDUCTION_MODE_MERGE_NV = 0,
       VK_COVERAGE_REDUCTION_MODE_TRUNCATE_NV = 1,
       VK_COVERAGE_REDUCTION_MODE_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkPipelineCoverageReductionStateCreateFlagsNV = ^VkPipelineCoverageReductionStateCreateFlagsNV;
     VkPipelineCoverageReductionStateCreateFlagsNV = VkFlags;
type P_VkPhysicalDeviceCoverageReductionModeFeaturesNV = ^VkPhysicalDeviceCoverageReductionModeFeaturesNV;
     VkPhysicalDeviceCoverageReductionModeFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       coverageReductionMode :VkBool32;
     end;

type P_VkPipelineCoverageReductionStateCreateInfoNV = ^VkPipelineCoverageReductionStateCreateInfoNV;
     VkPipelineCoverageReductionStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineCoverageReductionStateCreateFlagsNV;
       coverageReductionMode :VkCoverageReductionModeNV;
     end;

type P_VkFramebufferMixedSamplesCombinationNV = ^VkFramebufferMixedSamplesCombinationNV;
     VkFramebufferMixedSamplesCombinationNV = record
       sType :VkStructureType;
       pNext :P_void;
       coverageReductionMode :VkCoverageReductionModeNV;
       rasterizationSamples :VkSampleCountFlagBits;
       depthStencilSamples :VkSampleCountFlags;
       colorSamples :VkSampleCountFlags;
     end;

type PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV = function( physicalDevice_:VkPhysicalDevice; pCombinationCount_:P_uint32_t; pCombinations_:P_VkFramebufferMixedSamplesCombinationNV ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV(
    physicalDevice_:VkPhysicalDevice;
    pCombinationCount_:P_uint32_t;
    pCombinations_:P_VkFramebufferMixedSamplesCombinationNV ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_fragment_shader_interlock = 1;
const VK_EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION = 1;
const VK_EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME = 'VK_EXT_fragment_shader_interlock';
type P_VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT = ^VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT;
     VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentShaderSampleInterlock :VkBool32;
       fragmentShaderPixelInterlock :VkBool32;
       fragmentShaderShadingRateInterlock :VkBool32;
     end;



const VK_EXT_ycbcr_image_arrays = 1;
const VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1;
const VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = 'VK_EXT_ycbcr_image_arrays';
type P_VkPhysicalDeviceYcbcrImageArraysFeaturesEXT = ^VkPhysicalDeviceYcbcrImageArraysFeaturesEXT;
     VkPhysicalDeviceYcbcrImageArraysFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       ycbcrImageArrays :VkBool32;
     end;



const VK_EXT_headless_surface = 1;
const VK_EXT_HEADLESS_SURFACE_SPEC_VERSION = 1;
const VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME = 'VK_EXT_headless_surface';
type P_VkHeadlessSurfaceCreateFlagsEXT = ^VkHeadlessSurfaceCreateFlagsEXT;
     VkHeadlessSurfaceCreateFlagsEXT = VkFlags;
type P_VkHeadlessSurfaceCreateInfoEXT = ^VkHeadlessSurfaceCreateInfoEXT;
     VkHeadlessSurfaceCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkHeadlessSurfaceCreateFlagsEXT;
     end;

type PFN_vkCreateHeadlessSurfaceEXT = function( instance_:VkInstance; const pCreateInfo_:P_VkHeadlessSurfaceCreateInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateHeadlessSurfaceEXT(
    instance_:VkInstance;
    pCreateInfo_:P_VkHeadlessSurfaceCreateInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pSurface_:P_VkSurfaceKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_line_rasterization = 1;
const VK_EXT_LINE_RASTERIZATION_SPEC_VERSION = 1;
const VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME = 'VK_EXT_line_rasterization';

type P_VkLineRasterizationModeEXT = ^VkLineRasterizationModeEXT;
     VkLineRasterizationModeEXT = (
       VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT = 0,
       VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT = 1,
       VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT = 2,
       VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT = 3,
       VK_LINE_RASTERIZATION_MODE_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkPhysicalDeviceLineRasterizationFeaturesEXT = ^VkPhysicalDeviceLineRasterizationFeaturesEXT;
     VkPhysicalDeviceLineRasterizationFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       rectangularLines :VkBool32;
       bresenhamLines :VkBool32;
       smoothLines :VkBool32;
       stippledRectangularLines :VkBool32;
       stippledBresenhamLines :VkBool32;
       stippledSmoothLines :VkBool32;
     end;

type P_VkPhysicalDeviceLineRasterizationPropertiesEXT = ^VkPhysicalDeviceLineRasterizationPropertiesEXT;
     VkPhysicalDeviceLineRasterizationPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       lineSubPixelPrecisionBits :T_uint32_t;
     end;

type P_VkPipelineRasterizationLineStateCreateInfoEXT = ^VkPipelineRasterizationLineStateCreateInfoEXT;
     VkPipelineRasterizationLineStateCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       lineRasterizationMode :VkLineRasterizationModeEXT;
       stippledLineEnable :VkBool32;
       lineStippleFactor :T_uint32_t;
       lineStipplePattern :T_uint16_t;
     end;

type PFN_vkCmdSetLineStippleEXT = procedure( commandBuffer_:VkCommandBuffer; lineStippleFactor_:T_uint32_t; lineStipplePattern_:T_uint16_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetLineStippleEXT(
    commandBuffer_:VkCommandBuffer;
    lineStippleFactor_:T_uint32_t;
    lineStipplePattern_:T_uint16_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_shader_atomic_float = 1;
const VK_EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION = 1;
const VK_EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME = 'VK_EXT_shader_atomic_float';
type P_VkPhysicalDeviceShaderAtomicFloatFeaturesEXT = ^VkPhysicalDeviceShaderAtomicFloatFeaturesEXT;
     VkPhysicalDeviceShaderAtomicFloatFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       shaderBufferFloat32Atomics :VkBool32;
       shaderBufferFloat32AtomicAdd :VkBool32;
       shaderBufferFloat64Atomics :VkBool32;
       shaderBufferFloat64AtomicAdd :VkBool32;
       shaderSharedFloat32Atomics :VkBool32;
       shaderSharedFloat32AtomicAdd :VkBool32;
       shaderSharedFloat64Atomics :VkBool32;
       shaderSharedFloat64AtomicAdd :VkBool32;
       shaderImageFloat32Atomics :VkBool32;
       shaderImageFloat32AtomicAdd :VkBool32;
       sparseImageFloat32Atomics :VkBool32;
       sparseImageFloat32AtomicAdd :VkBool32;
     end;



const VK_EXT_host_query_reset = 1;
const VK_EXT_HOST_QUERY_RESET_SPEC_VERSION = 1;
const VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME = 'VK_EXT_host_query_reset';
type P_VkPhysicalDeviceHostQueryResetFeaturesEXT = ^VkPhysicalDeviceHostQueryResetFeaturesEXT;
     VkPhysicalDeviceHostQueryResetFeaturesEXT = VkPhysicalDeviceHostQueryResetFeatures;

type PFN_vkResetQueryPoolEXT = procedure( device_:VkDevice; queryPool_:VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkResetQueryPoolEXT(
    device_:VkDevice;
    queryPool_:VkQueryPool;
    firstQuery_:T_uint32_t;
    queryCount_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_index_type_uint8 = 1;
const VK_EXT_INDEX_TYPE_UINT8_SPEC_VERSION = 1;
const VK_EXT_INDEX_TYPE_UINT8_EXTENSION_NAME = 'VK_EXT_index_type_uint8';
type P_VkPhysicalDeviceIndexTypeUint8FeaturesEXT = ^VkPhysicalDeviceIndexTypeUint8FeaturesEXT;
     VkPhysicalDeviceIndexTypeUint8FeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       indexTypeUint8 :VkBool32;
     end;



const VK_EXT_extended_dynamic_state = 1;
const VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1;
const VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME = 'VK_EXT_extended_dynamic_state';
type P_VkPhysicalDeviceExtendedDynamicStateFeaturesEXT = ^VkPhysicalDeviceExtendedDynamicStateFeaturesEXT;
     VkPhysicalDeviceExtendedDynamicStateFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       extendedDynamicState :VkBool32;
     end;

type PFN_vkCmdSetCullModeEXT = procedure( commandBuffer_:VkCommandBuffer; cullMode_:VkCullModeFlags );
type PFN_vkCmdSetFrontFaceEXT = procedure( commandBuffer_:VkCommandBuffer; frontFace_:VkFrontFace );
type PFN_vkCmdSetPrimitiveTopologyEXT = procedure( commandBuffer_:VkCommandBuffer; primitiveTopology_:VkPrimitiveTopology );
type PFN_vkCmdSetViewportWithCountEXT = procedure( commandBuffer_:VkCommandBuffer; viewportCount_:T_uint32_t; const pViewports_:P_VkViewport );
type PFN_vkCmdSetScissorWithCountEXT = procedure( commandBuffer_:VkCommandBuffer; scissorCount_:T_uint32_t; const pScissors_:P_VkRect2D );
type PFN_vkCmdBindVertexBuffers2EXT = procedure( commandBuffer_:VkCommandBuffer; firstBinding_:T_uint32_t; bindingCount_:T_uint32_t; const pBuffers_:P_VkBuffer; const pOffsets_:P_VkDeviceSize; const pSizes_:P_VkDeviceSize; const pStrides_:P_VkDeviceSize );
type PFN_vkCmdSetDepthTestEnableEXT = procedure( commandBuffer_:VkCommandBuffer; depthTestEnable_:VkBool32 );
type PFN_vkCmdSetDepthWriteEnableEXT = procedure( commandBuffer_:VkCommandBuffer; depthWriteEnable_:VkBool32 );
type PFN_vkCmdSetDepthCompareOpEXT = procedure( commandBuffer_:VkCommandBuffer; depthCompareOp_:VkCompareOp );
type PFN_vkCmdSetDepthBoundsTestEnableEXT = procedure( commandBuffer_:VkCommandBuffer; depthBoundsTestEnable_:VkBool32 );
type PFN_vkCmdSetStencilTestEnableEXT = procedure( commandBuffer_:VkCommandBuffer; stencilTestEnable_:VkBool32 );
type PFN_vkCmdSetStencilOpEXT = procedure( commandBuffer_:VkCommandBuffer; faceMask_:VkStencilFaceFlags; failOp_:VkStencilOp; passOp_:VkStencilOp; depthFailOp_:VkStencilOp; compareOp_:VkCompareOp );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetCullModeEXT(
    commandBuffer_:VkCommandBuffer;
    cullMode_:VkCullModeFlags ); stdcall; external DLLNAME;

procedure vkCmdSetFrontFaceEXT(
    commandBuffer_:VkCommandBuffer;
    frontFace_:VkFrontFace ); stdcall; external DLLNAME;

procedure vkCmdSetPrimitiveTopologyEXT(
    commandBuffer_:VkCommandBuffer;
    primitiveTopology_:VkPrimitiveTopology ); stdcall; external DLLNAME;

procedure vkCmdSetViewportWithCountEXT(
    commandBuffer_:VkCommandBuffer;
    viewportCount_:T_uint32_t;
    pViewports_:P_VkViewport ); stdcall; external DLLNAME;

procedure vkCmdSetScissorWithCountEXT(
    commandBuffer_:VkCommandBuffer;
    scissorCount_:T_uint32_t;
    pScissors_:P_VkRect2D ); stdcall; external DLLNAME;

procedure vkCmdBindVertexBuffers2EXT(
    commandBuffer_:VkCommandBuffer;
    firstBinding_:T_uint32_t;
    bindingCount_:T_uint32_t;
    pBuffers_:P_VkBuffer;
    pOffsets_:P_VkDeviceSize;
    pSizes_:P_VkDeviceSize;
    pStrides_:P_VkDeviceSize ); stdcall; external DLLNAME;

procedure vkCmdSetDepthTestEnableEXT(
    commandBuffer_:VkCommandBuffer;
    depthTestEnable_:VkBool32 ); stdcall; external DLLNAME;

procedure vkCmdSetDepthWriteEnableEXT(
    commandBuffer_:VkCommandBuffer;
    depthWriteEnable_:VkBool32 ); stdcall; external DLLNAME;

procedure vkCmdSetDepthCompareOpEXT(
    commandBuffer_:VkCommandBuffer;
    depthCompareOp_:VkCompareOp ); stdcall; external DLLNAME;

procedure vkCmdSetDepthBoundsTestEnableEXT(
    commandBuffer_:VkCommandBuffer;
    depthBoundsTestEnable_:VkBool32 ); stdcall; external DLLNAME;

procedure vkCmdSetStencilTestEnableEXT(
    commandBuffer_:VkCommandBuffer;
    stencilTestEnable_:VkBool32 ); stdcall; external DLLNAME;

procedure vkCmdSetStencilOpEXT(
    commandBuffer_:VkCommandBuffer;
    faceMask_:VkStencilFaceFlags;
    failOp_:VkStencilOp;
    passOp_:VkStencilOp;
    depthFailOp_:VkStencilOp;
    compareOp_:VkCompareOp ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_shader_demote_to_helper_invocation = 1;
const VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION = 1;
const VK_EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME = 'VK_EXT_shader_demote_to_helper_invocation';
type P_VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = ^VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT;
     VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       shaderDemoteToHelperInvocation :VkBool32;
     end;



const VK_NV_device_generated_commands = 1;
type P_VkIndirectCommandsLayoutNV = ^VkIndirectCommandsLayoutNV;
     VkIndirectCommandsLayoutNV = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3;
const VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = 'VK_NV_device_generated_commands';

type P_VkIndirectCommandsTokenTypeNV = ^VkIndirectCommandsTokenTypeNV;
     VkIndirectCommandsTokenTypeNV = (
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV = 0,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV = 1,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV = 2,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV = 3,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV = 4,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV = 5,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV = 6,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV = 7,
       VK_INDIRECT_COMMANDS_TOKEN_TYPE_MAX_ENUM_NV = $7FFFFFFF
     );

type P_VkIndirectStateFlagBitsNV = ^VkIndirectStateFlagBitsNV;
     VkIndirectStateFlagBitsNV = (
       VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV = $00000001,
       VK_INDIRECT_STATE_FLAG_BITS_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkIndirectStateFlagsNV = ^VkIndirectStateFlagsNV;
     VkIndirectStateFlagsNV = VkFlags;

type P_VkIndirectCommandsLayoutUsageFlagBitsNV = ^VkIndirectCommandsLayoutUsageFlagBitsNV;
     VkIndirectCommandsLayoutUsageFlagBitsNV = (
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV = $00000001,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV = $00000002,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV = $00000004,
       VK_INDIRECT_COMMANDS_LAYOUT_USAGE_FLAG_BITS_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkIndirectCommandsLayoutUsageFlagsNV = ^VkIndirectCommandsLayoutUsageFlagsNV;
     VkIndirectCommandsLayoutUsageFlagsNV = VkFlags;
type P_VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV = ^VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV;
     VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       maxGraphicsShaderGroupCount :T_uint32_t;
       maxIndirectSequenceCount :T_uint32_t;
       maxIndirectCommandsTokenCount :T_uint32_t;
       maxIndirectCommandsStreamCount :T_uint32_t;
       maxIndirectCommandsTokenOffset :T_uint32_t;
       maxIndirectCommandsStreamStride :T_uint32_t;
       minSequencesCountBufferOffsetAlignment :T_uint32_t;
       minSequencesIndexBufferOffsetAlignment :T_uint32_t;
       minIndirectCommandsBufferOffsetAlignment :T_uint32_t;
     end;

type P_VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV = ^VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV;
     VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       deviceGeneratedCommands :VkBool32;
     end;

type P_VkGraphicsShaderGroupCreateInfoNV = ^VkGraphicsShaderGroupCreateInfoNV;
     VkGraphicsShaderGroupCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       stageCount :T_uint32_t;
       pStages :P_VkPipelineShaderStageCreateInfo;
       pVertexInputState :P_VkPipelineVertexInputStateCreateInfo;
       pTessellationState :P_VkPipelineTessellationStateCreateInfo;
     end;

type P_VkGraphicsPipelineShaderGroupsCreateInfoNV = ^VkGraphicsPipelineShaderGroupsCreateInfoNV;
     VkGraphicsPipelineShaderGroupsCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       groupCount :T_uint32_t;
       pGroups :P_VkGraphicsShaderGroupCreateInfoNV;
       pipelineCount :T_uint32_t;
       pPipelines :P_VkPipeline;
     end;

type P_VkBindShaderGroupIndirectCommandNV = ^VkBindShaderGroupIndirectCommandNV;
     VkBindShaderGroupIndirectCommandNV = record
       groupIndex :T_uint32_t;
     end;

type P_VkBindIndexBufferIndirectCommandNV = ^VkBindIndexBufferIndirectCommandNV;
     VkBindIndexBufferIndirectCommandNV = record
       bufferAddress :VkDeviceAddress;
       size :T_uint32_t;
       indexType :VkIndexType;
     end;

type P_VkBindVertexBufferIndirectCommandNV = ^VkBindVertexBufferIndirectCommandNV;
     VkBindVertexBufferIndirectCommandNV = record
       bufferAddress :VkDeviceAddress;
       size :T_uint32_t;
       stride :T_uint32_t;
     end;

type P_VkSetStateFlagsIndirectCommandNV = ^VkSetStateFlagsIndirectCommandNV;
     VkSetStateFlagsIndirectCommandNV = record
       data :T_uint32_t;
     end;

type P_VkIndirectCommandsStreamNV = ^VkIndirectCommandsStreamNV;
     VkIndirectCommandsStreamNV = record
       buffer :VkBuffer;
       offset :VkDeviceSize;
     end;

type P_VkIndirectCommandsLayoutTokenNV = ^VkIndirectCommandsLayoutTokenNV;
     VkIndirectCommandsLayoutTokenNV = record
       sType :VkStructureType;
       pNext :P_void;
       tokenType :VkIndirectCommandsTokenTypeNV;
       stream :T_uint32_t;
       offset :T_uint32_t;
       vertexBindingUnit :T_uint32_t;
       vertexDynamicStride :VkBool32;
       pushconstantPipelineLayout :VkPipelineLayout;
       pushconstantShaderStageFlags :VkShaderStageFlags;
       pushconstantOffset :T_uint32_t;
       pushconstantSize :T_uint32_t;
       indirectStateFlags :VkIndirectStateFlagsNV;
       indexTypeCount :T_uint32_t;
       pIndexTypes :P_VkIndexType;
       pIndexTypeValues :P_uint32_t;
     end;

type P_VkIndirectCommandsLayoutCreateInfoNV = ^VkIndirectCommandsLayoutCreateInfoNV;
     VkIndirectCommandsLayoutCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkIndirectCommandsLayoutUsageFlagsNV;
       pipelineBindPoint :VkPipelineBindPoint;
       tokenCount :T_uint32_t;
       pTokens :P_VkIndirectCommandsLayoutTokenNV;
       streamCount :T_uint32_t;
       pStreamStrides :P_uint32_t;
     end;

type P_VkGeneratedCommandsInfoNV = ^VkGeneratedCommandsInfoNV;
     VkGeneratedCommandsInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       pipelineBindPoint :VkPipelineBindPoint;
       pipeline :VkPipeline;
       indirectCommandsLayout :VkIndirectCommandsLayoutNV;
       streamCount :T_uint32_t;
       pStreams :P_VkIndirectCommandsStreamNV;
       sequencesCount :T_uint32_t;
       preprocessBuffer :VkBuffer;
       preprocessOffset :VkDeviceSize;
       preprocessSize :VkDeviceSize;
       sequencesCountBuffer :VkBuffer;
       sequencesCountOffset :VkDeviceSize;
       sequencesIndexBuffer :VkBuffer;
       sequencesIndexOffset :VkDeviceSize;
     end;

type P_VkGeneratedCommandsMemoryRequirementsInfoNV = ^VkGeneratedCommandsMemoryRequirementsInfoNV;
     VkGeneratedCommandsMemoryRequirementsInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       pipelineBindPoint :VkPipelineBindPoint;
       pipeline :VkPipeline;
       indirectCommandsLayout :VkIndirectCommandsLayoutNV;
       maxSequencesCount :T_uint32_t;
     end;

type PFN_vkGetGeneratedCommandsMemoryRequirementsNV = procedure( device_:VkDevice; const pInfo_:P_VkGeneratedCommandsMemoryRequirementsInfoNV; pMemoryRequirements_:P_VkMemoryRequirements2 );
type PFN_vkCmdPreprocessGeneratedCommandsNV = procedure( commandBuffer_:VkCommandBuffer; const pGeneratedCommandsInfo_:P_VkGeneratedCommandsInfoNV );
type PFN_vkCmdExecuteGeneratedCommandsNV = procedure( commandBuffer_:VkCommandBuffer; isPreprocessed_:VkBool32; const pGeneratedCommandsInfo_:P_VkGeneratedCommandsInfoNV );
type PFN_vkCmdBindPipelineShaderGroupNV = procedure( commandBuffer_:VkCommandBuffer; pipelineBindPoint_:VkPipelineBindPoint; pipeline_:VkPipeline; groupIndex_:T_uint32_t );
type PFN_vkCreateIndirectCommandsLayoutNV = function( device_:VkDevice; const pCreateInfo_:P_VkIndirectCommandsLayoutCreateInfoNV; const pAllocator_:P_VkAllocationCallbacks; pIndirectCommandsLayout_:P_VkIndirectCommandsLayoutNV ) :VkResult;
type PFN_vkDestroyIndirectCommandsLayoutNV = procedure( device_:VkDevice; indirectCommandsLayout_:VkIndirectCommandsLayoutNV; const pAllocator_:P_VkAllocationCallbacks );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkGetGeneratedCommandsMemoryRequirementsNV(
    device_:VkDevice;
    pInfo_:P_VkGeneratedCommandsMemoryRequirementsInfoNV;
    pMemoryRequirements_:P_VkMemoryRequirements2 ); stdcall; external DLLNAME;

procedure vkCmdPreprocessGeneratedCommandsNV(
    commandBuffer_:VkCommandBuffer;
    pGeneratedCommandsInfo_:P_VkGeneratedCommandsInfoNV ); stdcall; external DLLNAME;

procedure vkCmdExecuteGeneratedCommandsNV(
    commandBuffer_:VkCommandBuffer;
    isPreprocessed_:VkBool32;
    pGeneratedCommandsInfo_:P_VkGeneratedCommandsInfoNV ); stdcall; external DLLNAME;

procedure vkCmdBindPipelineShaderGroupNV(
    commandBuffer_:VkCommandBuffer;
    pipelineBindPoint_:VkPipelineBindPoint;
    pipeline_:VkPipeline;
    groupIndex_:T_uint32_t ); stdcall; external DLLNAME;

function vkCreateIndirectCommandsLayoutNV(
    device_:VkDevice;
    pCreateInfo_:P_VkIndirectCommandsLayoutCreateInfoNV;
    pAllocator_:P_VkAllocationCallbacks;
    pIndirectCommandsLayout_:P_VkIndirectCommandsLayoutNV ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyIndirectCommandsLayoutNV(
    device_:VkDevice;
    indirectCommandsLayout_:VkIndirectCommandsLayoutNV;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_texel_buffer_alignment = 1;
const VK_EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1;
const VK_EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = 'VK_EXT_texel_buffer_alignment';
type P_VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT = ^VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT;
     VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       texelBufferAlignment :VkBool32;
     end;

type P_VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT = ^VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT;
     VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       storageTexelBufferOffsetAlignmentBytes :VkDeviceSize;
       storageTexelBufferOffsetSingleTexelAlignment :VkBool32;
       uniformTexelBufferOffsetAlignmentBytes :VkDeviceSize;
       uniformTexelBufferOffsetSingleTexelAlignment :VkBool32;
     end;



const VK_QCOM_render_pass_transform = 1;
const VK_QCOM_RENDER_PASS_TRANSFORM_SPEC_VERSION = 1;
const VK_QCOM_RENDER_PASS_TRANSFORM_EXTENSION_NAME = 'VK_QCOM_render_pass_transform';
type P_VkRenderPassTransformBeginInfoQCOM = ^VkRenderPassTransformBeginInfoQCOM;
     VkRenderPassTransformBeginInfoQCOM = record
       sType :VkStructureType;
       pNext :P_void;
       transform :VkSurfaceTransformFlagBitsKHR;
     end;

type P_VkCommandBufferInheritanceRenderPassTransformInfoQCOM = ^VkCommandBufferInheritanceRenderPassTransformInfoQCOM;
     VkCommandBufferInheritanceRenderPassTransformInfoQCOM = record
       sType :VkStructureType;
       pNext :P_void;
       transform :VkSurfaceTransformFlagBitsKHR;
       renderArea :VkRect2D;
     end;



const VK_EXT_device_memory_report = 1;
const VK_EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION = 2;
const VK_EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME = 'VK_EXT_device_memory_report';

type P_VkDeviceMemoryReportEventTypeEXT = ^VkDeviceMemoryReportEventTypeEXT;
     VkDeviceMemoryReportEventTypeEXT = (
       VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATE_EXT = 0,
       VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_FREE_EXT = 1,
       VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_IMPORT_EXT = 2,
       VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_UNIMPORT_EXT = 3,
       VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_ALLOCATION_FAILED_EXT = 4,
       VK_DEVICE_MEMORY_REPORT_EVENT_TYPE_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkDeviceMemoryReportFlagsEXT = ^VkDeviceMemoryReportFlagsEXT;
     VkDeviceMemoryReportFlagsEXT = VkFlags;
type P_VkPhysicalDeviceDeviceMemoryReportFeaturesEXT = ^VkPhysicalDeviceDeviceMemoryReportFeaturesEXT;
     VkPhysicalDeviceDeviceMemoryReportFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       deviceMemoryReport :VkBool32;
     end;

type P_VkDeviceMemoryReportCallbackDataEXT = ^VkDeviceMemoryReportCallbackDataEXT;
     VkDeviceMemoryReportCallbackDataEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDeviceMemoryReportFlagsEXT;
       type_ :VkDeviceMemoryReportEventTypeEXT;
       memoryObjectId :T_uint64_t;
       size :VkDeviceSize;
       objectType :VkObjectType;
       objectHandle :T_uint64_t;
       heapIndex :T_uint32_t;
     end;

type PFN_vkDeviceMemoryReportCallbackEXT = procedure(
    pCallbackData_:P_VkDeviceMemoryReportCallbackDataEXT;
    pUserData_:P_void );

type P_VkDeviceDeviceMemoryReportCreateInfoEXT = ^VkDeviceDeviceMemoryReportCreateInfoEXT;
     VkDeviceDeviceMemoryReportCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDeviceMemoryReportFlagsEXT;
       pfnUserCallback :PFN_vkDeviceMemoryReportCallbackEXT;
       pUserData :P_void;
     end;



const VK_EXT_robustness2 = 1;
const VK_EXT_ROBUSTNESS_2_SPEC_VERSION  = 1;
const VK_EXT_ROBUSTNESS_2_EXTENSION_NAME = 'VK_EXT_robustness2';
type P_VkPhysicalDeviceRobustness2FeaturesEXT = ^VkPhysicalDeviceRobustness2FeaturesEXT;
     VkPhysicalDeviceRobustness2FeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       robustBufferAccess2 :VkBool32;
       robustImageAccess2 :VkBool32;
       nullDescriptor :VkBool32;
     end;

type P_VkPhysicalDeviceRobustness2PropertiesEXT = ^VkPhysicalDeviceRobustness2PropertiesEXT;
     VkPhysicalDeviceRobustness2PropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       robustStorageBufferAccessSizeAlignment :VkDeviceSize;
       robustUniformBufferAccessSizeAlignment :VkDeviceSize;
     end;



const VK_EXT_custom_border_color = 1;
const VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION = 12;
const VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME = 'VK_EXT_custom_border_color';
type P_VkSamplerCustomBorderColorCreateInfoEXT = ^VkSamplerCustomBorderColorCreateInfoEXT;
     VkSamplerCustomBorderColorCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       customBorderColor :VkClearColorValue;
       format :VkFormat;
     end;

type P_VkPhysicalDeviceCustomBorderColorPropertiesEXT = ^VkPhysicalDeviceCustomBorderColorPropertiesEXT;
     VkPhysicalDeviceCustomBorderColorPropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       maxCustomBorderColorSamplers :T_uint32_t;
     end;

type P_VkPhysicalDeviceCustomBorderColorFeaturesEXT = ^VkPhysicalDeviceCustomBorderColorFeaturesEXT;
     VkPhysicalDeviceCustomBorderColorFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       customBorderColors :VkBool32;
       customBorderColorWithoutFormat :VkBool32;
     end;



const VK_GOOGLE_user_type = 1;
const VK_GOOGLE_USER_TYPE_SPEC_VERSION  = 1;
const VK_GOOGLE_USER_TYPE_EXTENSION_NAME = 'VK_GOOGLE_user_type';


const VK_EXT_private_data = 1;
type P_VkPrivateDataSlotEXT = ^VkPrivateDataSlotEXT;
     VkPrivateDataSlotEXT = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_EXT_PRIVATE_DATA_SPEC_VERSION  = 1;
const VK_EXT_PRIVATE_DATA_EXTENSION_NAME = 'VK_EXT_private_data';

type P_VkPrivateDataSlotCreateFlagBitsEXT = ^VkPrivateDataSlotCreateFlagBitsEXT;
     VkPrivateDataSlotCreateFlagBitsEXT = (
       VK_PRIVATE_DATA_SLOT_CREATE_FLAG_BITS_MAX_ENUM_EXT = $7FFFFFFF
     );
type P_VkPrivateDataSlotCreateFlagsEXT = ^VkPrivateDataSlotCreateFlagsEXT;
     VkPrivateDataSlotCreateFlagsEXT = VkFlags;
type P_VkPhysicalDevicePrivateDataFeaturesEXT = ^VkPhysicalDevicePrivateDataFeaturesEXT;
     VkPhysicalDevicePrivateDataFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       privateData :VkBool32;
     end;

type P_VkDevicePrivateDataCreateInfoEXT = ^VkDevicePrivateDataCreateInfoEXT;
     VkDevicePrivateDataCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       privateDataSlotRequestCount :T_uint32_t;
     end;

type P_VkPrivateDataSlotCreateInfoEXT = ^VkPrivateDataSlotCreateInfoEXT;
     VkPrivateDataSlotCreateInfoEXT = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPrivateDataSlotCreateFlagsEXT;
     end;

type PFN_vkCreatePrivateDataSlotEXT = function( device_:VkDevice; const pCreateInfo_:P_VkPrivateDataSlotCreateInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pPrivateDataSlot_:P_VkPrivateDataSlotEXT ) :VkResult;
type PFN_vkDestroyPrivateDataSlotEXT = procedure( device_:VkDevice; privateDataSlot_:VkPrivateDataSlotEXT; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkSetPrivateDataEXT = function( device_:VkDevice; objectType_:VkObjectType; objectHandle_:T_uint64_t; privateDataSlot_:VkPrivateDataSlotEXT; data_:T_uint64_t ) :VkResult;
type PFN_vkGetPrivateDataEXT = procedure( device_:VkDevice; objectType_:VkObjectType; objectHandle_:T_uint64_t; privateDataSlot_:VkPrivateDataSlotEXT; pData_:P_uint64_t );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreatePrivateDataSlotEXT(
    device_:VkDevice;
    pCreateInfo_:P_VkPrivateDataSlotCreateInfoEXT;
    pAllocator_:P_VkAllocationCallbacks;
    pPrivateDataSlot_:P_VkPrivateDataSlotEXT ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyPrivateDataSlotEXT(
    device_:VkDevice;
    privateDataSlot_:VkPrivateDataSlotEXT;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkSetPrivateDataEXT(
    device_:VkDevice;
    objectType_:VkObjectType;
    objectHandle_:T_uint64_t;
    privateDataSlot_:VkPrivateDataSlotEXT;
    data_:T_uint64_t ) :VkResult; stdcall; external DLLNAME;

procedure vkGetPrivateDataEXT(
    device_:VkDevice;
    objectType_:VkObjectType;
    objectHandle_:T_uint64_t;
    privateDataSlot_:VkPrivateDataSlotEXT;
    pData_:P_uint64_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_pipeline_creation_cache_control = 1;
const VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3;
const VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME = 'VK_EXT_pipeline_creation_cache_control';
type P_VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT = ^VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT;
     VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       pipelineCreationCacheControl :VkBool32;
     end;



const VK_NV_device_diagnostics_config = 1;
const VK_NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION = 1;
const VK_NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME = 'VK_NV_device_diagnostics_config';

type P_VkDeviceDiagnosticsConfigFlagBitsNV = ^VkDeviceDiagnosticsConfigFlagBitsNV;
     VkDeviceDiagnosticsConfigFlagBitsNV = (
       VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_DEBUG_INFO_BIT_NV = $00000001,
       VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_RESOURCE_TRACKING_BIT_NV = $00000002,
       VK_DEVICE_DIAGNOSTICS_CONFIG_ENABLE_AUTOMATIC_CHECKPOINTS_BIT_NV = $00000004,
       VK_DEVICE_DIAGNOSTICS_CONFIG_FLAG_BITS_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkDeviceDiagnosticsConfigFlagsNV = ^VkDeviceDiagnosticsConfigFlagsNV;
     VkDeviceDiagnosticsConfigFlagsNV = VkFlags;
type P_VkPhysicalDeviceDiagnosticsConfigFeaturesNV = ^VkPhysicalDeviceDiagnosticsConfigFeaturesNV;
     VkPhysicalDeviceDiagnosticsConfigFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       diagnosticsConfig :VkBool32;
     end;

type P_VkDeviceDiagnosticsConfigCreateInfoNV = ^VkDeviceDiagnosticsConfigCreateInfoNV;
     VkDeviceDiagnosticsConfigCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkDeviceDiagnosticsConfigFlagsNV;
     end;



const VK_QCOM_render_pass_store_ops = 1;
const VK_QCOM_render_pass_store_ops_SPEC_VERSION = 2;
const VK_QCOM_render_pass_store_ops_EXTENSION_NAME = 'VK_QCOM_render_pass_store_ops';


const VK_NV_fragment_shading_rate_enums = 1;
const VK_NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION = 1;
const VK_NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME = 'VK_NV_fragment_shading_rate_enums';

type P_VkFragmentShadingRateTypeNV = ^VkFragmentShadingRateTypeNV;
     VkFragmentShadingRateTypeNV = (
       VK_FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV = 0,
       VK_FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV = 1,
       VK_FRAGMENT_SHADING_RATE_TYPE_MAX_ENUM_NV = $7FFFFFFF
     );

type P_VkFragmentShadingRateNV = ^VkFragmentShadingRateNV;
     VkFragmentShadingRateNV = (
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV = 0,
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV = 1,
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV = 4,
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV = 5,
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV = 6,
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV = 9,
       VK_FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV = 10,
       VK_FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV = 11,
       VK_FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV = 12,
       VK_FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV = 13,
       VK_FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV = 14,
       VK_FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV = 15,
       VK_FRAGMENT_SHADING_RATE_MAX_ENUM_NV = $7FFFFFFF
     );
type P_VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV = ^VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV;
     VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentShadingRateEnums :VkBool32;
       supersampleFragmentShadingRates :VkBool32;
       noInvocationFragmentShadingRates :VkBool32;
     end;

type P_VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV = ^VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV;
     VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV = record
       sType :VkStructureType;
       pNext :P_void;
       maxFragmentShadingRateInvocationCount :VkSampleCountFlagBits;
     end;

type P_VkPipelineFragmentShadingRateEnumStateCreateInfoNV = ^VkPipelineFragmentShadingRateEnumStateCreateInfoNV;
     VkPipelineFragmentShadingRateEnumStateCreateInfoNV = record
       sType :VkStructureType;
       pNext :P_void;
       shadingRateType :VkFragmentShadingRateTypeNV;
       shadingRate :VkFragmentShadingRateNV;
       combinerOps :array [ 0..2-1 ] of VkFragmentShadingRateCombinerOpKHR;
     end;

type PFN_vkCmdSetFragmentShadingRateEnumNV = procedure( commandBuffer_:VkCommandBuffer; shadingRate_:VkFragmentShadingRateNV; const combinerOps_:T_combinerOps );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdSetFragmentShadingRateEnumNV(
    commandBuffer_:VkCommandBuffer;
    shadingRate_:VkFragmentShadingRateNV;
    const combinerOps_:T_combinerOps ); stdcall; external DLLNAME;
{$ENDIF}


const VK_EXT_fragment_density_map2 = 1;
const VK_EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION = 1;
const VK_EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME = 'VK_EXT_fragment_density_map2';
type P_VkPhysicalDeviceFragmentDensityMap2FeaturesEXT = ^VkPhysicalDeviceFragmentDensityMap2FeaturesEXT;
     VkPhysicalDeviceFragmentDensityMap2FeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       fragmentDensityMapDeferred :VkBool32;
     end;

type P_VkPhysicalDeviceFragmentDensityMap2PropertiesEXT = ^VkPhysicalDeviceFragmentDensityMap2PropertiesEXT;
     VkPhysicalDeviceFragmentDensityMap2PropertiesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       subsampledLoads :VkBool32;
       subsampledCoarseReconstructionEarlyAccess :VkBool32;
       maxSubsampledArrayLayers :T_uint32_t;
       maxDescriptorSetSubsampledSamplers :T_uint32_t;
     end;



const VK_QCOM_rotated_copy_commands = 1;
const VK_QCOM_ROTATED_COPY_COMMANDS_SPEC_VERSION = 0;
const VK_QCOM_ROTATED_COPY_COMMANDS_EXTENSION_NAME = 'VK_QCOM_rotated_copy_commands';
type P_VkCopyCommandTransformInfoQCOM = ^VkCopyCommandTransformInfoQCOM;
     VkCopyCommandTransformInfoQCOM = record
       sType :VkStructureType;
       pNext :P_void;
       transform :VkSurfaceTransformFlagBitsKHR;
     end;



const VK_EXT_image_robustness = 1;
const VK_EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1;
const VK_EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = 'VK_EXT_image_robustness';
type P_VkPhysicalDeviceImageRobustnessFeaturesEXT = ^VkPhysicalDeviceImageRobustnessFeaturesEXT;
     VkPhysicalDeviceImageRobustnessFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       robustImageAccess :VkBool32;
     end;



const VK_EXT_4444_formats = 1;
const VK_EXT_4444_FORMATS_SPEC_VERSION  = 1;
const VK_EXT_4444_FORMATS_EXTENSION_NAME = 'VK_EXT_4444_formats';
type P_VkPhysicalDevice4444FormatsFeaturesEXT = ^VkPhysicalDevice4444FormatsFeaturesEXT;
     VkPhysicalDevice4444FormatsFeaturesEXT = record
       sType :VkStructureType;
       pNext :P_void;
       formatA4R4G4B4 :VkBool32;
       formatA4B4G4R4 :VkBool32;
     end;



const VK_NV_acquire_winrt_display = 1;
const VK_NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION = 1;
const VK_NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME = 'VK_NV_acquire_winrt_display';
type PFN_vkAcquireWinrtDisplayNV = function( physicalDevice_:VkPhysicalDevice; display_:VkDisplayKHR ) :VkResult;
type PFN_vkGetWinrtDisplayNV = function( physicalDevice_:VkPhysicalDevice; deviceRelativeId_:T_uint32_t; pDisplay_:P_VkDisplayKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkAcquireWinrtDisplayNV(
    physicalDevice_:VkPhysicalDevice;
    display_:VkDisplayKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetWinrtDisplayNV(
    physicalDevice_:VkPhysicalDevice;
    deviceRelativeId_:T_uint32_t;
    pDisplay_:P_VkDisplayKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_VALVE_mutable_descriptor_type = 1;
const VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1;
const VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = 'VK_VALVE_mutable_descriptor_type';
type P_VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE = ^VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE;
     VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE = record
       sType :VkStructureType;
       pNext :P_void;
       mutableDescriptorType :VkBool32;
     end;

type P_VkMutableDescriptorTypeListVALVE = ^VkMutableDescriptorTypeListVALVE;
     VkMutableDescriptorTypeListVALVE = record
       descriptorTypeCount :T_uint32_t;
       pDescriptorTypes :P_VkDescriptorType;
     end;

type P_VkMutableDescriptorTypeCreateInfoVALVE = ^VkMutableDescriptorTypeCreateInfoVALVE;
     VkMutableDescriptorTypeCreateInfoVALVE = record
       sType :VkStructureType;
       pNext :P_void;
       mutableDescriptorTypeListCount :T_uint32_t;
       pMutableDescriptorTypeLists :P_VkMutableDescriptorTypeListVALVE;
     end;



const VK_KHR_acceleration_structure = 1;
type P_VkAccelerationStructureKHR = ^VkAccelerationStructureKHR;
     VkAccelerationStructureKHR = VK_DEFINE_NON_DISPATCHABLE_HANDLE;
const VK_KHR_ACCELERATION_STRUCTURE_SPEC_VERSION = 11;
const VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME = 'VK_KHR_acceleration_structure';

type P_VkBuildAccelerationStructureModeKHR = ^VkBuildAccelerationStructureModeKHR;
     VkBuildAccelerationStructureModeKHR = (
       VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR = 0,
       VK_BUILD_ACCELERATION_STRUCTURE_MODE_UPDATE_KHR = 1,
       VK_BUILD_ACCELERATION_STRUCTURE_MODE_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkAccelerationStructureBuildTypeKHR = ^VkAccelerationStructureBuildTypeKHR;
     VkAccelerationStructureBuildTypeKHR = (
       VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR = 0,
       VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR = 1,
       VK_ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR = 2,
       VK_ACCELERATION_STRUCTURE_BUILD_TYPE_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkAccelerationStructureCompatibilityKHR = ^VkAccelerationStructureCompatibilityKHR;
     VkAccelerationStructureCompatibilityKHR = (
       VK_ACCELERATION_STRUCTURE_COMPATIBILITY_COMPATIBLE_KHR = 0,
       VK_ACCELERATION_STRUCTURE_COMPATIBILITY_INCOMPATIBLE_KHR = 1,
       VK_ACCELERATION_STRUCTURE_COMPATIBILITY_MAX_ENUM_KHR = $7FFFFFFF
     );

type P_VkAccelerationStructureCreateFlagBitsKHR = ^VkAccelerationStructureCreateFlagBitsKHR;
     VkAccelerationStructureCreateFlagBitsKHR = (
       VK_ACCELERATION_STRUCTURE_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR = $00000001,
       VK_ACCELERATION_STRUCTURE_CREATE_FLAG_BITS_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkAccelerationStructureCreateFlagsKHR = ^VkAccelerationStructureCreateFlagsKHR;
     VkAccelerationStructureCreateFlagsKHR = VkFlags;
type P_VkDeviceOrHostAddressKHR = ^VkDeviceOrHostAddressKHR;
     VkDeviceOrHostAddressKHR = record
     case Byte of
       0:( deviceAddress :VkDeviceAddress );
       1:( hostAddress   :P_void          );
     end;

type P_VkDeviceOrHostAddressConstKHR = ^VkDeviceOrHostAddressConstKHR;
     VkDeviceOrHostAddressConstKHR = record
     case Byte of
       0:( deviceAddress :VkDeviceAddress );
       1:( hostAddress   :P_void          );
     end;

type PP_VkAccelerationStructureBuildRangeInfoKHR = ^P_VkAccelerationStructureBuildRangeInfoKHR;
     P_VkAccelerationStructureBuildRangeInfoKHR = ^VkAccelerationStructureBuildRangeInfoKHR;
     VkAccelerationStructureBuildRangeInfoKHR = record
       primitiveCount :T_uint32_t;
       primitiveOffset :T_uint32_t;
       firstVertex :T_uint32_t;
       transformOffset :T_uint32_t;
     end;

type P_VkAccelerationStructureGeometryTrianglesDataKHR = ^VkAccelerationStructureGeometryTrianglesDataKHR;
     VkAccelerationStructureGeometryTrianglesDataKHR = record
       sType :VkStructureType;
       pNext :P_void;
       vertexFormat :VkFormat;
       vertexData :VkDeviceOrHostAddressConstKHR;
       vertexStride :VkDeviceSize;
       maxVertex :T_uint32_t;
       indexType :VkIndexType;
       indexData :VkDeviceOrHostAddressConstKHR;
       transformData :VkDeviceOrHostAddressConstKHR;
     end;

type P_VkAccelerationStructureGeometryAabbsDataKHR = ^VkAccelerationStructureGeometryAabbsDataKHR;
     VkAccelerationStructureGeometryAabbsDataKHR = record
       sType :VkStructureType;
       pNext :P_void;
       data :VkDeviceOrHostAddressConstKHR;
       stride :VkDeviceSize;
     end;

type P_VkAccelerationStructureGeometryInstancesDataKHR = ^VkAccelerationStructureGeometryInstancesDataKHR;
     VkAccelerationStructureGeometryInstancesDataKHR = record
       sType :VkStructureType;
       pNext :P_void;
       arrayOfPointers :VkBool32;
       data :VkDeviceOrHostAddressConstKHR;
     end;

type P_VkAccelerationStructureGeometryDataKHR = ^VkAccelerationStructureGeometryDataKHR;
     VkAccelerationStructureGeometryDataKHR = record
     case Byte of
       0:( triangles :VkAccelerationStructureGeometryTrianglesDataKHR );
       1:( aabbs     :VkAccelerationStructureGeometryAabbsDataKHR     );
       2:( instances :VkAccelerationStructureGeometryInstancesDataKHR );
     end;

type PP_VkAccelerationStructureGeometryKHR = ^P_VkAccelerationStructureGeometryKHR;
     P_VkAccelerationStructureGeometryKHR = ^VkAccelerationStructureGeometryKHR;
     VkAccelerationStructureGeometryKHR = record
       sType :VkStructureType;
       pNext :P_void;
       geometryType :VkGeometryTypeKHR;
       geometry :VkAccelerationStructureGeometryDataKHR;
       flags :VkGeometryFlagsKHR;
     end;

type P_VkAccelerationStructureBuildGeometryInfoKHR = ^VkAccelerationStructureBuildGeometryInfoKHR;
     VkAccelerationStructureBuildGeometryInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkAccelerationStructureTypeKHR;
       flags :VkBuildAccelerationStructureFlagsKHR;
       mode :VkBuildAccelerationStructureModeKHR;
       srcAccelerationStructure :VkAccelerationStructureKHR;
       dstAccelerationStructure :VkAccelerationStructureKHR;
       geometryCount :T_uint32_t;
       pGeometries :P_VkAccelerationStructureGeometryKHR;
       ppGeometries :PP_VkAccelerationStructureGeometryKHR;
       scratchData :VkDeviceOrHostAddressKHR;
     end;

type P_VkAccelerationStructureCreateInfoKHR = ^VkAccelerationStructureCreateInfoKHR;
     VkAccelerationStructureCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       createFlags :VkAccelerationStructureCreateFlagsKHR;
       buffer :VkBuffer;
       offset :VkDeviceSize;
       size :VkDeviceSize;
       type_ :VkAccelerationStructureTypeKHR;
       deviceAddress :VkDeviceAddress;
     end;

type P_VkWriteDescriptorSetAccelerationStructureKHR = ^VkWriteDescriptorSetAccelerationStructureKHR;
     VkWriteDescriptorSetAccelerationStructureKHR = record
       sType :VkStructureType;
       pNext :P_void;
       accelerationStructureCount :T_uint32_t;
       pAccelerationStructures :P_VkAccelerationStructureKHR;
     end;

type P_VkPhysicalDeviceAccelerationStructureFeaturesKHR = ^VkPhysicalDeviceAccelerationStructureFeaturesKHR;
     VkPhysicalDeviceAccelerationStructureFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       accelerationStructure :VkBool32;
       accelerationStructureCaptureReplay :VkBool32;
       accelerationStructureIndirectBuild :VkBool32;
       accelerationStructureHostCommands :VkBool32;
       descriptorBindingAccelerationStructureUpdateAfterBind :VkBool32;
     end;

type P_VkPhysicalDeviceAccelerationStructurePropertiesKHR = ^VkPhysicalDeviceAccelerationStructurePropertiesKHR;
     VkPhysicalDeviceAccelerationStructurePropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       maxGeometryCount :T_uint64_t;
       maxInstanceCount :T_uint64_t;
       maxPrimitiveCount :T_uint64_t;
       maxPerStageDescriptorAccelerationStructures :T_uint32_t;
       maxPerStageDescriptorUpdateAfterBindAccelerationStructures :T_uint32_t;
       maxDescriptorSetAccelerationStructures :T_uint32_t;
       maxDescriptorSetUpdateAfterBindAccelerationStructures :T_uint32_t;
       minAccelerationStructureScratchOffsetAlignment :T_uint32_t;
     end;

type P_VkAccelerationStructureDeviceAddressInfoKHR = ^VkAccelerationStructureDeviceAddressInfoKHR;
     VkAccelerationStructureDeviceAddressInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       accelerationStructure :VkAccelerationStructureKHR;
     end;

type P_VkAccelerationStructureVersionInfoKHR = ^VkAccelerationStructureVersionInfoKHR;
     VkAccelerationStructureVersionInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       pVersionData :P_uint8_t;
     end;

type P_VkCopyAccelerationStructureToMemoryInfoKHR = ^VkCopyAccelerationStructureToMemoryInfoKHR;
     VkCopyAccelerationStructureToMemoryInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       src :VkAccelerationStructureKHR;
       dst :VkDeviceOrHostAddressKHR;
       mode :VkCopyAccelerationStructureModeKHR;
     end;

type P_VkCopyMemoryToAccelerationStructureInfoKHR = ^VkCopyMemoryToAccelerationStructureInfoKHR;
     VkCopyMemoryToAccelerationStructureInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       src :VkDeviceOrHostAddressConstKHR;
       dst :VkAccelerationStructureKHR;
       mode :VkCopyAccelerationStructureModeKHR;
     end;

type P_VkCopyAccelerationStructureInfoKHR = ^VkCopyAccelerationStructureInfoKHR;
     VkCopyAccelerationStructureInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       src :VkAccelerationStructureKHR;
       dst :VkAccelerationStructureKHR;
       mode :VkCopyAccelerationStructureModeKHR;
     end;

type P_VkAccelerationStructureBuildSizesInfoKHR = ^VkAccelerationStructureBuildSizesInfoKHR;
     VkAccelerationStructureBuildSizesInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       accelerationStructureSize :VkDeviceSize;
       updateScratchSize :VkDeviceSize;
       buildScratchSize :VkDeviceSize;
     end;

type PFN_vkCreateAccelerationStructureKHR = function( device_:VkDevice; const pCreateInfo_:P_VkAccelerationStructureCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pAccelerationStructure_:P_VkAccelerationStructureKHR ) :VkResult;
type PFN_vkDestroyAccelerationStructureKHR = procedure( device_:VkDevice; accelerationStructure_:VkAccelerationStructureKHR; const pAllocator_:P_VkAllocationCallbacks );
type PFN_vkCmdBuildAccelerationStructuresKHR = procedure( commandBuffer_:VkCommandBuffer; infoCount_:T_uint32_t; const pInfos_:P_VkAccelerationStructureBuildGeometryInfoKHR; const ppBuildRangeInfos_:PP_VkAccelerationStructureBuildRangeInfoKHR );
type PFN_vkCmdBuildAccelerationStructuresIndirectKHR = procedure( commandBuffer_:VkCommandBuffer; infoCount_:T_uint32_t; const pInfos_:P_VkAccelerationStructureBuildGeometryInfoKHR; const pIndirectDeviceAddresses_:P_VkDeviceAddress; const pIndirectStrides_:P_uint32_t; const ppMaxPrimitiveCounts_:PP_uint32_t );
type PFN_vkBuildAccelerationStructuresKHR = function( device_:VkDevice; deferredOperation_:VkDeferredOperationKHR; infoCount_:T_uint32_t; const pInfos_:P_VkAccelerationStructureBuildGeometryInfoKHR; const ppBuildRangeInfos_:PP_VkAccelerationStructureBuildRangeInfoKHR ) :VkResult;
type PFN_vkCopyAccelerationStructureKHR = function( device_:VkDevice; deferredOperation_:VkDeferredOperationKHR; const pInfo_:P_VkCopyAccelerationStructureInfoKHR ) :VkResult;
type PFN_vkCopyAccelerationStructureToMemoryKHR = function( device_:VkDevice; deferredOperation_:VkDeferredOperationKHR; const pInfo_:P_VkCopyAccelerationStructureToMemoryInfoKHR ) :VkResult;
type PFN_vkCopyMemoryToAccelerationStructureKHR = function( device_:VkDevice; deferredOperation_:VkDeferredOperationKHR; const pInfo_:P_VkCopyMemoryToAccelerationStructureInfoKHR ) :VkResult;
type PFN_vkWriteAccelerationStructuresPropertiesKHR = function( device_:VkDevice; accelerationStructureCount_:T_uint32_t; const pAccelerationStructures_:P_VkAccelerationStructureKHR; queryType_:VkQueryType; dataSize_:T_size_t; pData_:P_void; stride_:T_size_t ) :VkResult;
type PFN_vkCmdCopyAccelerationStructureKHR = procedure( commandBuffer_:VkCommandBuffer; const pInfo_:P_VkCopyAccelerationStructureInfoKHR );
type PFN_vkCmdCopyAccelerationStructureToMemoryKHR = procedure( commandBuffer_:VkCommandBuffer; const pInfo_:P_VkCopyAccelerationStructureToMemoryInfoKHR );
type PFN_vkCmdCopyMemoryToAccelerationStructureKHR = procedure( commandBuffer_:VkCommandBuffer; const pInfo_:P_VkCopyMemoryToAccelerationStructureInfoKHR );
type PFN_vkGetAccelerationStructureDeviceAddressKHR = function( device_:VkDevice; const pInfo_:P_VkAccelerationStructureDeviceAddressInfoKHR ) :VkDeviceAddress;
type PFN_vkCmdWriteAccelerationStructuresPropertiesKHR = procedure( commandBuffer_:VkCommandBuffer; accelerationStructureCount_:T_uint32_t; const pAccelerationStructures_:P_VkAccelerationStructureKHR; queryType_:VkQueryType; queryPool_:VkQueryPool; firstQuery_:T_uint32_t );
type PFN_vkGetDeviceAccelerationStructureCompatibilityKHR = procedure( device_:VkDevice; const pVersionInfo_:P_VkAccelerationStructureVersionInfoKHR; pCompatibility_:P_VkAccelerationStructureCompatibilityKHR );
type PFN_vkGetAccelerationStructureBuildSizesKHR = procedure( device_:VkDevice; buildType_:VkAccelerationStructureBuildTypeKHR; const pBuildInfo_:P_VkAccelerationStructureBuildGeometryInfoKHR; const pMaxPrimitiveCounts_:P_uint32_t; pSizeInfo_:P_VkAccelerationStructureBuildSizesInfoKHR );

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateAccelerationStructureKHR(
    device_:VkDevice;
    pCreateInfo_:P_VkAccelerationStructureCreateInfoKHR;
    pAllocator_:P_VkAllocationCallbacks;
    pAccelerationStructure_:P_VkAccelerationStructureKHR ) :VkResult; stdcall; external DLLNAME;

procedure vkDestroyAccelerationStructureKHR(
    device_:VkDevice;
    accelerationStructure_:VkAccelerationStructureKHR;
    pAllocator_:P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkCmdBuildAccelerationStructuresKHR(
    commandBuffer_:VkCommandBuffer;
    infoCount_:T_uint32_t;
    pInfos_:P_VkAccelerationStructureBuildGeometryInfoKHR;
    ppBuildRangeInfos_:PP_VkAccelerationStructureBuildRangeInfoKHR ); stdcall; external DLLNAME;

procedure vkCmdBuildAccelerationStructuresIndirectKHR(
    commandBuffer_:VkCommandBuffer;
    infoCount_:T_uint32_t;
    pInfos_:P_VkAccelerationStructureBuildGeometryInfoKHR;
    pIndirectDeviceAddresses_:P_VkDeviceAddress;
    pIndirectStrides_:P_uint32_t;
    ppMaxPrimitiveCounts_:PP_uint32_t ); stdcall; external DLLNAME;

function vkBuildAccelerationStructuresKHR(
    device_:VkDevice;
    deferredOperation_:VkDeferredOperationKHR;
    infoCount_:T_uint32_t;
    pInfos_:P_VkAccelerationStructureBuildGeometryInfoKHR;
    ppBuildRangeInfos_:PP_VkAccelerationStructureBuildRangeInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkCopyAccelerationStructureKHR(
    device_:VkDevice;
    deferredOperation_:VkDeferredOperationKHR;
    pInfo_:P_VkCopyAccelerationStructureInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkCopyAccelerationStructureToMemoryKHR(
    device_:VkDevice;
    deferredOperation_:VkDeferredOperationKHR;
    pInfo_:P_VkCopyAccelerationStructureToMemoryInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkCopyMemoryToAccelerationStructureKHR(
    device_:VkDevice;
    deferredOperation_:VkDeferredOperationKHR;
    pInfo_:P_VkCopyMemoryToAccelerationStructureInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkWriteAccelerationStructuresPropertiesKHR(
    device_:VkDevice;
    accelerationStructureCount_:T_uint32_t;
    pAccelerationStructures_:P_VkAccelerationStructureKHR;
    queryType_:VkQueryType;
    dataSize_:T_size_t;
    pData_:P_void;
    stride_:T_size_t ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdCopyAccelerationStructureKHR(
    commandBuffer_:VkCommandBuffer;
    pInfo_:P_VkCopyAccelerationStructureInfoKHR ); stdcall; external DLLNAME;

procedure vkCmdCopyAccelerationStructureToMemoryKHR(
    commandBuffer_:VkCommandBuffer;
    pInfo_:P_VkCopyAccelerationStructureToMemoryInfoKHR ); stdcall; external DLLNAME;

procedure vkCmdCopyMemoryToAccelerationStructureKHR(
    commandBuffer_:VkCommandBuffer;
    pInfo_:P_VkCopyMemoryToAccelerationStructureInfoKHR ); stdcall; external DLLNAME;

function vkGetAccelerationStructureDeviceAddressKHR(
    device_:VkDevice;
    pInfo_:P_VkAccelerationStructureDeviceAddressInfoKHR ) :VkDeviceAddress; stdcall; external DLLNAME;

procedure vkCmdWriteAccelerationStructuresPropertiesKHR(
    commandBuffer_:VkCommandBuffer;
    accelerationStructureCount_:T_uint32_t;
    pAccelerationStructures_:P_VkAccelerationStructureKHR;
    queryType_:VkQueryType;
    queryPool_:VkQueryPool;
    firstQuery_:T_uint32_t ); stdcall; external DLLNAME;

procedure vkGetDeviceAccelerationStructureCompatibilityKHR(
    device_:VkDevice;
    pVersionInfo_:P_VkAccelerationStructureVersionInfoKHR;
    pCompatibility_:P_VkAccelerationStructureCompatibilityKHR ); stdcall; external DLLNAME;

procedure vkGetAccelerationStructureBuildSizesKHR(
    device_:VkDevice;
    buildType_:VkAccelerationStructureBuildTypeKHR;
    pBuildInfo_:P_VkAccelerationStructureBuildGeometryInfoKHR;
    pMaxPrimitiveCounts_:P_uint32_t;
    pSizeInfo_:P_VkAccelerationStructureBuildSizesInfoKHR ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_ray_tracing_pipeline = 1;
const VK_KHR_RAY_TRACING_PIPELINE_SPEC_VERSION = 1;
const VK_KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME = 'VK_KHR_ray_tracing_pipeline';

type P_VkShaderGroupShaderKHR = ^VkShaderGroupShaderKHR;
     VkShaderGroupShaderKHR = (
       VK_SHADER_GROUP_SHADER_GENERAL_KHR = 0,
       VK_SHADER_GROUP_SHADER_CLOSEST_HIT_KHR = 1,
       VK_SHADER_GROUP_SHADER_ANY_HIT_KHR = 2,
       VK_SHADER_GROUP_SHADER_INTERSECTION_KHR = 3,
       VK_SHADER_GROUP_SHADER_MAX_ENUM_KHR = $7FFFFFFF
     );
type P_VkRayTracingShaderGroupCreateInfoKHR = ^VkRayTracingShaderGroupCreateInfoKHR;
     VkRayTracingShaderGroupCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       type_ :VkRayTracingShaderGroupTypeKHR;
       generalShader :T_uint32_t;
       closestHitShader :T_uint32_t;
       anyHitShader :T_uint32_t;
       intersectionShader :T_uint32_t;
       pShaderGroupCaptureReplayHandle :P_void;
     end;

type P_VkRayTracingPipelineInterfaceCreateInfoKHR = ^VkRayTracingPipelineInterfaceCreateInfoKHR;
     VkRayTracingPipelineInterfaceCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       maxPipelineRayPayloadSize :T_uint32_t;
       maxPipelineRayHitAttributeSize :T_uint32_t;
     end;

type P_VkRayTracingPipelineCreateInfoKHR = ^VkRayTracingPipelineCreateInfoKHR;
     VkRayTracingPipelineCreateInfoKHR = record
       sType :VkStructureType;
       pNext :P_void;
       flags :VkPipelineCreateFlags;
       stageCount :T_uint32_t;
       pStages :P_VkPipelineShaderStageCreateInfo;
       groupCount :T_uint32_t;
       pGroups :P_VkRayTracingShaderGroupCreateInfoKHR;
       maxPipelineRayRecursionDepth :T_uint32_t;
       pLibraryInfo :P_VkPipelineLibraryCreateInfoKHR;
       pLibraryInterface :P_VkRayTracingPipelineInterfaceCreateInfoKHR;
       pDynamicState :P_VkPipelineDynamicStateCreateInfo;
       layout :VkPipelineLayout;
       basePipelineHandle :VkPipeline;
       basePipelineIndex :T_int32_t;
     end;

type P_VkPhysicalDeviceRayTracingPipelineFeaturesKHR = ^VkPhysicalDeviceRayTracingPipelineFeaturesKHR;
     VkPhysicalDeviceRayTracingPipelineFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       rayTracingPipeline :VkBool32;
       rayTracingPipelineShaderGroupHandleCaptureReplay :VkBool32;
       rayTracingPipelineShaderGroupHandleCaptureReplayMixed :VkBool32;
       rayTracingPipelineTraceRaysIndirect :VkBool32;
       rayTraversalPrimitiveCulling :VkBool32;
     end;

type P_VkPhysicalDeviceRayTracingPipelinePropertiesKHR = ^VkPhysicalDeviceRayTracingPipelinePropertiesKHR;
     VkPhysicalDeviceRayTracingPipelinePropertiesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       shaderGroupHandleSize :T_uint32_t;
       maxRayRecursionDepth :T_uint32_t;
       maxShaderGroupStride :T_uint32_t;
       shaderGroupBaseAlignment :T_uint32_t;
       shaderGroupHandleCaptureReplaySize :T_uint32_t;
       maxRayDispatchInvocationCount :T_uint32_t;
       shaderGroupHandleAlignment :T_uint32_t;
       maxRayHitAttributeSize :T_uint32_t;
     end;

type P_VkStridedDeviceAddressRegionKHR = ^VkStridedDeviceAddressRegionKHR;
     VkStridedDeviceAddressRegionKHR = record
       deviceAddress :VkDeviceAddress;
       stride :VkDeviceSize;
       size :VkDeviceSize;
     end;

type P_VkTraceRaysIndirectCommandKHR = ^VkTraceRaysIndirectCommandKHR;
     VkTraceRaysIndirectCommandKHR = record
       width :T_uint32_t;
       height :T_uint32_t;
       depth :T_uint32_t;
     end;

type PFN_vkCmdTraceRaysKHR = procedure( commandBuffer_:VkCommandBuffer; const pRaygenShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; const pMissShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; const pHitShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; const pCallableShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; width_:T_uint32_t; height_:T_uint32_t; depth_:T_uint32_t );
type PFN_vkCreateRayTracingPipelinesKHR = function( device_:VkDevice; deferredOperation_:VkDeferredOperationKHR; pipelineCache_:VkPipelineCache; createInfoCount_:T_uint32_t; const pCreateInfos_:P_VkRayTracingPipelineCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pPipelines_:P_VkPipeline ) :VkResult;
type PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR = function( device_:VkDevice; pipeline_:VkPipeline; firstGroup_:T_uint32_t; groupCount_:T_uint32_t; dataSize_:T_size_t; pData_:P_void ) :VkResult;
type PFN_vkCmdTraceRaysIndirectKHR = procedure( commandBuffer_:VkCommandBuffer; const pRaygenShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; const pMissShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; const pHitShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; const pCallableShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR; indirectDeviceAddress_:VkDeviceAddress );
type PFN_vkGetRayTracingShaderGroupStackSizeKHR = function( device_:VkDevice; pipeline_:VkPipeline; group_:T_uint32_t; groupShader_:VkShaderGroupShaderKHR ) :VkDeviceSize;
type PFN_vkCmdSetRayTracingPipelineStackSizeKHR = procedure( commandBuffer_:VkCommandBuffer; pipelineStackSize_:T_uint32_t );

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkCmdTraceRaysKHR(
    commandBuffer_:VkCommandBuffer;
    pRaygenShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    pMissShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    pHitShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    pCallableShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    width_:T_uint32_t;
    height_:T_uint32_t;
    depth_:T_uint32_t ); stdcall; external DLLNAME;

function vkCreateRayTracingPipelinesKHR(
    device_:VkDevice;
    deferredOperation_:VkDeferredOperationKHR;
    pipelineCache_:VkPipelineCache;
    createInfoCount_:T_uint32_t;
    pCreateInfos_:P_VkRayTracingPipelineCreateInfoKHR;
    pAllocator_:P_VkAllocationCallbacks;
    pPipelines_:P_VkPipeline ) :VkResult; stdcall; external DLLNAME;

function vkGetRayTracingCaptureReplayShaderGroupHandlesKHR(
    device_:VkDevice;
    pipeline_:VkPipeline;
    firstGroup_:T_uint32_t;
    groupCount_:T_uint32_t;
    dataSize_:T_size_t;
    pData_:P_void ) :VkResult; stdcall; external DLLNAME;

procedure vkCmdTraceRaysIndirectKHR(
    commandBuffer_:VkCommandBuffer;
    pRaygenShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    pMissShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    pHitShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    pCallableShaderBindingTable_:P_VkStridedDeviceAddressRegionKHR;
    indirectDeviceAddress_:VkDeviceAddress ); stdcall; external DLLNAME;

function vkGetRayTracingShaderGroupStackSizeKHR(
    device_:VkDevice;
    pipeline_:VkPipeline;
    group_:T_uint32_t;
    groupShader_:VkShaderGroupShaderKHR ) :VkDeviceSize; stdcall; external DLLNAME;

procedure vkCmdSetRayTracingPipelineStackSizeKHR(
    commandBuffer_:VkCommandBuffer;
    pipelineStackSize_:T_uint32_t ); stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_ray_query = 1;
const VK_KHR_RAY_QUERY_SPEC_VERSION     = 1;
const VK_KHR_RAY_QUERY_EXTENSION_NAME = 'VK_KHR_ray_query';
type P_VkPhysicalDeviceRayQueryFeaturesKHR = ^VkPhysicalDeviceRayQueryFeaturesKHR;
     VkPhysicalDeviceRayQueryFeaturesKHR = record
       sType :VkStructureType;
       pNext :P_void;
       rayQuery :VkBool32;
     end;


implementation //############################################################### ■

function VK_MAKE_VERSION( const major_,minor_,patch_:T_uint32_t ) :T_uint32_t;
begin
     Result := ( major_ shl 22 ) or ( minor_ shl 12 ) or patch_;
end;

//------------------------------------------------------------------------------

function VK_VERSION_MAJOR( const version_:T_uint32_t ) :T_uint32_t;
begin
     Result := version_ shr 22;
end;

function VK_VERSION_MINOR( const version_:T_uint32_t ) :T_uint32_t;
begin
     Result := ( version_ shr 12 ) and $3ff;
end;

function VK_VERSION_PATCH( const version_:T_uint32_t ) :T_uint32_t;
begin
     Result := version_ and $fff;
end;

end. //######################################################################### ■
