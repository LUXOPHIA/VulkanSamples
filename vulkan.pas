unit vulkan;

interface //#################################################################### ■

(*
** Copyright (c) 2015-2016 The Khronos Group Inc.
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
**
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
*)

(*
** This header is generated from the Khronos Vulkan XML API Registry.
**
*)

uses LUX, LUX.Code.C;

{$IFDEF MSWINDOWS}

const DLLNAME = 'vulkan-1.dll';

{$DEFINE VK_USE_PLATFORM_WIN32_KHR }

{$ENDIF}

type T_float4 = array [ 0..3 ] of T_float;

////////////////////////////////////////////////////////////////////////////////

const VK_VERSION_1_0 = 1;

function VK_MAKE_VERSION( const major_,minor_,patch_:Cardinal ) :Cardinal; inline;

// Vulkan API version supported by this file
var VK_API_VERSION :Cardinal;

function VK_VERSION_MAJOR( version_:Cardinal ) :Cardinal; inline;
function VK_VERSION_MINOR( version_:Cardinal ) :Cardinal; inline;
function VK_VERSION_PATCH( version_:Cardinal ) :Cardinal; inline;

const VK_NULL_HANDLE = 0;

type T_VkFlags      = T_uint32_t;  P_VkFlags      = ^T_VkFlags;
type T_VkBool32     = T_uint32_t;  P_VkBool32     = ^T_VkBool32;
type T_VkDeviceSize = T_uint64_t;  P_VkDeviceSize = ^T_VkDeviceSize;
type T_VkSampleMask = T_uint32_t;  P_VkSampleMask = ^T_VkSampleMask;

type T_VkInstance            = ^VkInstance_T           ; VkInstance_T            = record end;
type T_VkPhysicalDevice      = ^VkPhysicalDevice_T     ; VkPhysicalDevice_T      = record end;
type T_VkDevice              = ^VkDevice_T             ; VkDevice_T              = record end;
type T_VkQueue               = ^VkQueue_T              ; VkQueue_T               = record end;
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or Defined( __x86_64__ ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
type T_VkSemaphore           = ^VkSemaphore_T          ; VkSemaphore_T           = record end;
{$ELSE}
type T_VkSemaphore           = T_uint64_t;
{$ENDIF}
type T_VkCommandBuffer       = ^VkCommandBuffer_T      ; VkCommandBuffer_T       = record end;
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or Defined( __x86_64__ ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
type T_VkFence               = ^VkFence_T              ; VkFence_T               = record end;
type T_VkDeviceMemory        = ^VkDeviceMemory_T       ; VkDeviceMemory_T        = record end;
type T_VkBuffer              = ^VkBuffer_T             ; VkBuffer_T              = record end;
type T_VkImage               = ^VkImage_T              ; VkImage_T               = record end;
type T_VkEvent               = ^VkEvent_T              ; VkEvent_T               = record end;
type T_VkQueryPool           = ^VkQueryPool_T          ; VkQueryPool_T           = record end;
type T_VkBufferView          = ^VkBufferView_T         ; VkBufferView_T          = record end;
type T_VkImageView           = ^VkImageView_T          ; VkImageView_T           = record end;
type T_VkShaderModule        = ^VkShaderModule_T       ; VkShaderModule_T        = record end;
type T_VkPipelineCache       = ^VkPipelineCache_T      ; VkPipelineCache_T       = record end;
type T_VkPipelineLayout      = ^VkPipelineLayout_T     ; VkPipelineLayout_T      = record end;
type T_VkRenderPass          = ^VkRenderPass_T         ; VkRenderPass_T          = record end;
type T_VkPipeline            = ^VkPipeline_T           ; VkPipeline_T            = record end;
type T_VkDescriptorSetLayout = ^VkDescriptorSetLayout_T; VkDescriptorSetLayout_T = record end;
type T_VkSampler             = ^VkSampler_T            ; VkSampler_T             = record end;
type T_VkDescriptorPool      = ^VkDescriptorPool_T     ; VkDescriptorPool_T      = record end;
type T_VkDescriptorSet       = ^VkDescriptorSet_T      ; VkDescriptorSet_T       = record end;
type T_VkFramebuffer         = ^VkFramebuffer_T        ; VkFramebuffer_T         = record end;
type T_VkCommandPool         = ^VkCommandPool_T        ; VkCommandPool_T         = record end;
{$ELSE}
type T_VkFence               = T_uint64_t;
type T_VkDeviceMemory        = T_uint64_t;
type T_VkBuffer              = T_uint64_t;
type T_VkImage               = T_uint64_t;
type T_VkEvent               = T_uint64_t;
type T_VkQueryPool           = T_uint64_t;
type T_VkBufferView          = T_uint64_t;
type T_VkImageView           = T_uint64_t;
type T_VkShaderModule        = T_uint64_t;
type T_VkPipelineCache       = T_uint64_t;
type T_VkPipelineLayout      = T_uint64_t;
type T_VkRenderPass          = T_uint64_t;
type T_VkPipeline            = T_uint64_t;
type T_VkDescriptorSetLayout = T_uint64_t;
type T_VkSampler             = T_uint64_t;
type T_VkDescriptorPool      = T_uint64_t;
type T_VkDescriptorSet       = T_uint64_t;
type T_VkFramebuffer         = T_uint64_t;
type T_VkCommandPool         = T_uint64_t;
{$ENDIF}
type P_VkInstance            = ^T_VkInstance;
type P_VkPhysicalDevice      = ^T_VkPhysicalDevice;
type P_VkDevice              = ^T_VkDevice;
type P_VkQueue               = ^T_VkQueue;
type P_VkSemaphore           = ^T_VkSemaphore;
type p_VkCommandBuffer       = ^T_VkCommandBuffer;
type P_VkFence               = ^T_VkFence;
type P_VkDeviceMemory        = ^T_VkDeviceMemory;
type P_VkBuffer              = ^T_VkBuffer;
type P_VkImage               = ^T_VkImage;
type P_VkEvent               = ^T_VkEvent;
type P_VkQueryPool           = ^T_VkQueryPool;
type P_VkBufferView          = ^T_VkBufferView;
type P_VkImageView           = ^T_VkImageView;
type P_VkShaderModule        = ^T_VkShaderModule;
type P_VkPipelineCache       = ^T_VkPipelineCache;
type P_VkPipelineLayout      = ^T_VkPipelineLayout;
type P_VkRenderPass          = ^T_VkRenderPass;
type P_VkPipeline            = ^T_VkPipeline;
type P_VkDescriptorSetLayout = ^T_VkDescriptorSetLayout;
type P_VkSampler             = ^T_VkSampler;
type P_VkDescriptorPool      = ^T_VkDescriptorPool;
type P_VkDescriptorSet       = ^T_VkDescriptorSet;
type P_VkFramebuffer         = ^T_VkFramebuffer;
type P_VkCommandPool         = ^T_VkCommandPool;

const VK_LOD_CLAMP_NONE                = 1000;
const VK_REMAINING_MIP_LEVELS          = $FFFFFFFF;
const VK_REMAINING_ARRAY_LAYERS        = $FFFFFFFF;
const VK_WHOLE_SIZE                    = $FFFFFFFFFFFFFFFF;
const VK_ATTACHMENT_UNUSED             = $FFFFFFFF;
const VK_TRUE                          = 1;
const VK_FALSE                         = 0;
const VK_QUEUE_FAMILY_IGNORED          = $FFFFFFFF;
const VK_SUBPASS_EXTERNAL              = $FFFFFFFF;
const VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256;
const VK_UUID_SIZE                     = 16;
const VK_MAX_MEMORY_TYPES              = 32;
const VK_MAX_MEMORY_HEAPS              = 16;
const VK_MAX_EXTENSION_NAME_SIZE       = 256;
const VK_MAX_DESCRIPTION_SIZE          = 256;

type P_VkPipelineCacheHeaderVersion = ^T_VkPipelineCacheHeaderVersion;
     T_VkPipelineCacheHeaderVersion = (
       VK_PIPELINE_CACHE_HEADER_VERSION_ONE         = 1,
       VK_PIPELINE_CACHE_HEADER_VERSION_BEGIN_RANGE = VK_PIPELINE_CACHE_HEADER_VERSION_ONE,
       VK_PIPELINE_CACHE_HEADER_VERSION_END_RANGE   = VK_PIPELINE_CACHE_HEADER_VERSION_ONE,
       VK_PIPELINE_CACHE_HEADER_VERSION_RANGE_SIZE  = ( VK_PIPELINE_CACHE_HEADER_VERSION_ONE - VK_PIPELINE_CACHE_HEADER_VERSION_ONE + 1 ),
       VK_PIPELINE_CACHE_HEADER_VERSION_MAX_ENUM    = $7FFFFFFF
     );

type P_VkResult = ^T_VkResult;
     T_VkResult = (
       VK_SUCCESS                        = 0,
       VK_NOT_READY                      = 1,
       VK_TIMEOUT                        = 2,
       VK_EVENT_SET                      = 3,
       VK_EVENT_RESET                    = 4,
       VK_INCOMPLETE                     = 5,
       VK_ERROR_OUT_OF_HOST_MEMORY       = -1,
       VK_ERROR_OUT_OF_DEVICE_MEMORY     = -2,
       VK_ERROR_INITIALIZATION_FAILED    = -3,
       VK_ERROR_DEVICE_LOST              = -4,
       VK_ERROR_MEMORY_MAP_FAILED        = -5,
       VK_ERROR_LAYER_NOT_PRESENT        = -6,
       VK_ERROR_EXTENSION_NOT_PRESENT    = -7,
       VK_ERROR_FEATURE_NOT_PRESENT      = -8,
       VK_ERROR_INCOMPATIBLE_DRIVER      = -9,
       VK_ERROR_TOO_MANY_OBJECTS         = -10,
       VK_ERROR_FORMAT_NOT_SUPPORTED     = -11,
       VK_ERROR_SURFACE_LOST_KHR         = -1000000000,
       VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = -1000000001,
       VK_SUBOPTIMAL_KHR                 = 1000001003,
       VK_ERROR_OUT_OF_DATE_KHR          = -1000001004,
       VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = -1000003001,
       VK_ERROR_VALIDATION_FAILED_EXT    = -1000011001,
       VK_RESULT_BEGIN_RANGE             = VK_ERROR_FORMAT_NOT_SUPPORTED,
       VK_RESULT_END_RANGE               = VK_INCOMPLETE,
       VK_RESULT_RANGE_SIZE              = ( VK_INCOMPLETE - VK_ERROR_FORMAT_NOT_SUPPORTED + 1 ),
       VK_RESULT_MAX_ENUM                = $7FFFFFFF
     );

type P_VkStructureType = ^T_VkStructureType;
     T_VkStructureType = (
       VK_STRUCTURE_TYPE_APPLICATION_INFO                          = 0,
       VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO                      = 1,
       VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO                  = 2,
       VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO                        = 3,
       VK_STRUCTURE_TYPE_SUBMIT_INFO                               = 4,
       VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO                      = 5,
       VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE                       = 6,
       VK_STRUCTURE_TYPE_BIND_SPARSE_INFO                          = 7,
       VK_STRUCTURE_TYPE_FENCE_CREATE_INFO                         = 8,
       VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO                     = 9,
       VK_STRUCTURE_TYPE_EVENT_CREATE_INFO                         = 10,
       VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO                    = 11,
       VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO                        = 12,
       VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO                   = 13,
       VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO                         = 14,
       VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO                    = 15,
       VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO                 = 16,
       VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO                = 17,
       VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO         = 18,
       VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO   = 19,
       VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = 20,
       VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO   = 21,
       VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO       = 22,
       VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO  = 23,
       VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO    = 24,
       VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO  = 25,
       VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO    = 26,
       VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO        = 27,
       VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO             = 28,
       VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO              = 29,
       VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO               = 30,
       VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO                       = 31,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO         = 32,
       VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO               = 33,
       VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO              = 34,
       VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET                      = 35,
       VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET                       = 36,
       VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO                   = 37,
       VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO                   = 38,
       VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO                  = 39,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO              = 40,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO           = 41,
       VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO                 = 42,
       VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO                    = 43,
       VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER                     = 44,
       VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER                      = 45,
       VK_STRUCTURE_TYPE_MEMORY_BARRIER                            = 46,
       VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO               = 47,
       VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO                 = 48,
       VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR                 = 1000001000,
       VK_STRUCTURE_TYPE_PRESENT_INFO_KHR                          = 1000001001,
       VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR              = 1000002000,
       VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR           = 1000002001,
       VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR                  = 1000003000,
       VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR              = 1000004000,
       VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR               = 1000005000,
       VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR           = 1000006000,
       VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR               = 1000007000,
       VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR           = 1000008000,
       VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR             = 1000009000,
       VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT              = 1000011000,
       VK_STRUCTURE_TYPE_BEGIN_RANGE                               = VK_STRUCTURE_TYPE_APPLICATION_INFO,
       VK_STRUCTURE_TYPE_END_RANGE                                 = VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO,
       VK_STRUCTURE_TYPE_RANGE_SIZE                                = ( VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO - VK_STRUCTURE_TYPE_APPLICATION_INFO + 1 ),
       VK_STRUCTURE_TYPE_MAX_ENUM                                  = $7FFFFFFF
     );

type P_VkSystemAllocationScope = ^T_VkSystemAllocationScope;
     T_VkSystemAllocationScope = (
       VK_SYSTEM_ALLOCATION_SCOPE_COMMAND     = 0,
       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT      = 1,
       VK_SYSTEM_ALLOCATION_SCOPE_CACHE       = 2,
       VK_SYSTEM_ALLOCATION_SCOPE_DEVICE      = 3,
       VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE    = 4,
       VK_SYSTEM_ALLOCATION_SCOPE_BEGIN_RANGE = VK_SYSTEM_ALLOCATION_SCOPE_COMMAND,
       VK_SYSTEM_ALLOCATION_SCOPE_END_RANGE   = VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE,
       VK_SYSTEM_ALLOCATION_SCOPE_RANGE_SIZE  = ( VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE - VK_SYSTEM_ALLOCATION_SCOPE_COMMAND + 1 ),
       VK_SYSTEM_ALLOCATION_SCOPE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkInternalAllocationType = ^T_VkInternalAllocationType;
     T_VkInternalAllocationType = (
       VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE  = 0,
       VK_INTERNAL_ALLOCATION_TYPE_BEGIN_RANGE = VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE,
       VK_INTERNAL_ALLOCATION_TYPE_END_RANGE   = VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE,
       VK_INTERNAL_ALLOCATION_TYPE_RANGE_SIZE  = ( VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE - VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE + 1 ),
       VK_INTERNAL_ALLOCATION_TYPE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkFormat = ^T_VkFormat;
     T_VkFormat = (
       VK_FORMAT_UNDEFINED                  =   0,
       VK_FORMAT_R4G4_UNORM_PACK8           =   1,
       VK_FORMAT_R4G4B4A4_UNORM_PACK16      =   2,
       VK_FORMAT_B4G4R4A4_UNORM_PACK16      =   3,
       VK_FORMAT_R5G6B5_UNORM_PACK16        =   4,
       VK_FORMAT_B5G6R5_UNORM_PACK16        =   5,
       VK_FORMAT_R5G5B5A1_UNORM_PACK16      =   6,
       VK_FORMAT_B5G5R5A1_UNORM_PACK16      =   7,
       VK_FORMAT_A1R5G5B5_UNORM_PACK16      =   8,
       VK_FORMAT_R8_UNORM                   =   9,
       VK_FORMAT_R8_SNORM                   =  10,
       VK_FORMAT_R8_USCALED                 =  11,
       VK_FORMAT_R8_SSCALED                 =  12,
       VK_FORMAT_R8_UINT                    =  13,
       VK_FORMAT_R8_SINT                    =  14,
       VK_FORMAT_R8_SRGB                    =  15,
       VK_FORMAT_R8G8_UNORM                 =  16,
       VK_FORMAT_R8G8_SNORM                 =  17,
       VK_FORMAT_R8G8_USCALED               =  18,
       VK_FORMAT_R8G8_SSCALED               =  19,
       VK_FORMAT_R8G8_UINT                  =  20,
       VK_FORMAT_R8G8_SINT                  =  21,
       VK_FORMAT_R8G8_SRGB                  =  22,
       VK_FORMAT_R8G8B8_UNORM               =  23,
       VK_FORMAT_R8G8B8_SNORM               =  24,
       VK_FORMAT_R8G8B8_USCALED             =  25,
       VK_FORMAT_R8G8B8_SSCALED             =  26,
       VK_FORMAT_R8G8B8_UINT                =  27,
       VK_FORMAT_R8G8B8_SINT                =  28,
       VK_FORMAT_R8G8B8_SRGB                =  29,
       VK_FORMAT_B8G8R8_UNORM               =  30,
       VK_FORMAT_B8G8R8_SNORM               =  31,
       VK_FORMAT_B8G8R8_USCALED             =  32,
       VK_FORMAT_B8G8R8_SSCALED             =  33,
       VK_FORMAT_B8G8R8_UINT                =  34,
       VK_FORMAT_B8G8R8_SINT                =  35,
       VK_FORMAT_B8G8R8_SRGB                =  36,
       VK_FORMAT_R8G8B8A8_UNORM             =  37,
       VK_FORMAT_R8G8B8A8_SNORM             =  38,
       VK_FORMAT_R8G8B8A8_USCALED           =  39,
       VK_FORMAT_R8G8B8A8_SSCALED           =  40,
       VK_FORMAT_R8G8B8A8_UINT              =  41,
       VK_FORMAT_R8G8B8A8_SINT              =  42,
       VK_FORMAT_R8G8B8A8_SRGB              =  43,
       VK_FORMAT_B8G8R8A8_UNORM             =  44,
       VK_FORMAT_B8G8R8A8_SNORM             =  45,
       VK_FORMAT_B8G8R8A8_USCALED           =  46,
       VK_FORMAT_B8G8R8A8_SSCALED           =  47,
       VK_FORMAT_B8G8R8A8_UINT              =  48,
       VK_FORMAT_B8G8R8A8_SINT              =  49,
       VK_FORMAT_B8G8R8A8_SRGB              =  50,
       VK_FORMAT_A8B8G8R8_UNORM_PACK32      =  51,
       VK_FORMAT_A8B8G8R8_SNORM_PACK32      =  52,
       VK_FORMAT_A8B8G8R8_USCALED_PACK32    =  53,
       VK_FORMAT_A8B8G8R8_SSCALED_PACK32    =  54,
       VK_FORMAT_A8B8G8R8_UINT_PACK32       =  55,
       VK_FORMAT_A8B8G8R8_SINT_PACK32       =  56,
       VK_FORMAT_A8B8G8R8_SRGB_PACK32       =  57,
       VK_FORMAT_A2R10G10B10_UNORM_PACK32   =  58,
       VK_FORMAT_A2R10G10B10_SNORM_PACK32   =  59,
       VK_FORMAT_A2R10G10B10_USCALED_PACK32 =  60,
       VK_FORMAT_A2R10G10B10_SSCALED_PACK32 =  61,
       VK_FORMAT_A2R10G10B10_UINT_PACK32    =  62,
       VK_FORMAT_A2R10G10B10_SINT_PACK32    =  63,
       VK_FORMAT_A2B10G10R10_UNORM_PACK32   =  64,
       VK_FORMAT_A2B10G10R10_SNORM_PACK32   =  65,
       VK_FORMAT_A2B10G10R10_USCALED_PACK32 =  66,
       VK_FORMAT_A2B10G10R10_SSCALED_PACK32 =  67,
       VK_FORMAT_A2B10G10R10_UINT_PACK32    =  68,
       VK_FORMAT_A2B10G10R10_SINT_PACK32    =  69,
       VK_FORMAT_R16_UNORM                  =  70,
       VK_FORMAT_R16_SNORM                  =  71,
       VK_FORMAT_R16_USCALED                =  72,
       VK_FORMAT_R16_SSCALED                =  73,
       VK_FORMAT_R16_UINT                   =  74,
       VK_FORMAT_R16_SINT                   =  75,
       VK_FORMAT_R16_SFLOAT                 =  76,
       VK_FORMAT_R16G16_UNORM               =  77,
       VK_FORMAT_R16G16_SNORM               =  78,
       VK_FORMAT_R16G16_USCALED             =  79,
       VK_FORMAT_R16G16_SSCALED             =  80,
       VK_FORMAT_R16G16_UINT                =  81,
       VK_FORMAT_R16G16_SINT                =  82,
       VK_FORMAT_R16G16_SFLOAT              =  83,
       VK_FORMAT_R16G16B16_UNORM            =  84,
       VK_FORMAT_R16G16B16_SNORM            =  85,
       VK_FORMAT_R16G16B16_USCALED          =  86,
       VK_FORMAT_R16G16B16_SSCALED          =  87,
       VK_FORMAT_R16G16B16_UINT             =  88,
       VK_FORMAT_R16G16B16_SINT             =  89,
       VK_FORMAT_R16G16B16_SFLOAT           =  90,
       VK_FORMAT_R16G16B16A16_UNORM         =  91,
       VK_FORMAT_R16G16B16A16_SNORM         =  92,
       VK_FORMAT_R16G16B16A16_USCALED       =  93,
       VK_FORMAT_R16G16B16A16_SSCALED       =  94,
       VK_FORMAT_R16G16B16A16_UINT          =  95,
       VK_FORMAT_R16G16B16A16_SINT          =  96,
       VK_FORMAT_R16G16B16A16_SFLOAT        =  97,
       VK_FORMAT_R32_UINT                   =  98,
       VK_FORMAT_R32_SINT                   =  99,
       VK_FORMAT_R32_SFLOAT                 = 100,
       VK_FORMAT_R32G32_UINT                = 101,
       VK_FORMAT_R32G32_SINT                = 102,
       VK_FORMAT_R32G32_SFLOAT              = 103,
       VK_FORMAT_R32G32B32_UINT             = 104,
       VK_FORMAT_R32G32B32_SINT             = 105,
       VK_FORMAT_R32G32B32_SFLOAT           = 106,
       VK_FORMAT_R32G32B32A32_UINT          = 107,
       VK_FORMAT_R32G32B32A32_SINT          = 108,
       VK_FORMAT_R32G32B32A32_SFLOAT        = 109,
       VK_FORMAT_R64_UINT                   = 110,
       VK_FORMAT_R64_SINT                   = 111,
       VK_FORMAT_R64_SFLOAT                 = 112,
       VK_FORMAT_R64G64_UINT                = 113,
       VK_FORMAT_R64G64_SINT                = 114,
       VK_FORMAT_R64G64_SFLOAT              = 115,
       VK_FORMAT_R64G64B64_UINT             = 116,
       VK_FORMAT_R64G64B64_SINT             = 117,
       VK_FORMAT_R64G64B64_SFLOAT           = 118,
       VK_FORMAT_R64G64B64A64_UINT          = 119,
       VK_FORMAT_R64G64B64A64_SINT          = 120,
       VK_FORMAT_R64G64B64A64_SFLOAT        = 121,
       VK_FORMAT_B10G11R11_UFLOAT_PACK32    = 122,
       VK_FORMAT_E5B9G9R9_UFLOAT_PACK32     = 123,
       VK_FORMAT_D16_UNORM                  = 124,
       VK_FORMAT_X8_D24_UNORM_PACK32        = 125,
       VK_FORMAT_D32_SFLOAT                 = 126,
       VK_FORMAT_S8_UINT                    = 127,
       VK_FORMAT_D16_UNORM_S8_UINT          = 128,
       VK_FORMAT_D24_UNORM_S8_UINT          = 129,
       VK_FORMAT_D32_SFLOAT_S8_UINT         = 130,
       VK_FORMAT_BC1_RGB_UNORM_BLOCK        = 131,
       VK_FORMAT_BC1_RGB_SRGB_BLOCK         = 132,
       VK_FORMAT_BC1_RGBA_UNORM_BLOCK       = 133,
       VK_FORMAT_BC1_RGBA_SRGB_BLOCK        = 134,
       VK_FORMAT_BC2_UNORM_BLOCK            = 135,
       VK_FORMAT_BC2_SRGB_BLOCK             = 136,
       VK_FORMAT_BC3_UNORM_BLOCK            = 137,
       VK_FORMAT_BC3_SRGB_BLOCK             = 138,
       VK_FORMAT_BC4_UNORM_BLOCK            = 139,
       VK_FORMAT_BC4_SNORM_BLOCK            = 140,
       VK_FORMAT_BC5_UNORM_BLOCK            = 141,
       VK_FORMAT_BC5_SNORM_BLOCK            = 142,
       VK_FORMAT_BC6H_UFLOAT_BLOCK          = 143,
       VK_FORMAT_BC6H_SFLOAT_BLOCK          = 144,
       VK_FORMAT_BC7_UNORM_BLOCK            = 145,
       VK_FORMAT_BC7_SRGB_BLOCK             = 146,
       VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK    = 147,
       VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK     = 148,
       VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK  = 149,
       VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK   = 150,
       VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK  = 151,
       VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK   = 152,
       VK_FORMAT_EAC_R11_UNORM_BLOCK        = 153,
       VK_FORMAT_EAC_R11_SNORM_BLOCK        = 154,
       VK_FORMAT_EAC_R11G11_UNORM_BLOCK     = 155,
       VK_FORMAT_EAC_R11G11_SNORM_BLOCK     = 156,
       VK_FORMAT_ASTC_4x4_UNORM_BLOCK       = 157,
       VK_FORMAT_ASTC_4x4_SRGB_BLOCK        = 158,
       VK_FORMAT_ASTC_5x4_UNORM_BLOCK       = 159,
       VK_FORMAT_ASTC_5x4_SRGB_BLOCK        = 160,
       VK_FORMAT_ASTC_5x5_UNORM_BLOCK       = 161,
       VK_FORMAT_ASTC_5x5_SRGB_BLOCK        = 162,
       VK_FORMAT_ASTC_6x5_UNORM_BLOCK       = 163,
       VK_FORMAT_ASTC_6x5_SRGB_BLOCK        = 164,
       VK_FORMAT_ASTC_6x6_UNORM_BLOCK       = 165,
       VK_FORMAT_ASTC_6x6_SRGB_BLOCK        = 166,
       VK_FORMAT_ASTC_8x5_UNORM_BLOCK       = 167,
       VK_FORMAT_ASTC_8x5_SRGB_BLOCK        = 168,
       VK_FORMAT_ASTC_8x6_UNORM_BLOCK       = 169,
       VK_FORMAT_ASTC_8x6_SRGB_BLOCK        = 170,
       VK_FORMAT_ASTC_8x8_UNORM_BLOCK       = 171,
       VK_FORMAT_ASTC_8x8_SRGB_BLOCK        = 172,
       VK_FORMAT_ASTC_10x5_UNORM_BLOCK      = 173,
       VK_FORMAT_ASTC_10x5_SRGB_BLOCK       = 174,
       VK_FORMAT_ASTC_10x6_UNORM_BLOCK      = 175,
       VK_FORMAT_ASTC_10x6_SRGB_BLOCK       = 176,
       VK_FORMAT_ASTC_10x8_UNORM_BLOCK      = 177,
       VK_FORMAT_ASTC_10x8_SRGB_BLOCK       = 178,
       VK_FORMAT_ASTC_10x10_UNORM_BLOCK     = 179,
       VK_FORMAT_ASTC_10x10_SRGB_BLOCK      = 180,
       VK_FORMAT_ASTC_12x10_UNORM_BLOCK     = 181,
       VK_FORMAT_ASTC_12x10_SRGB_BLOCK      = 182,
       VK_FORMAT_ASTC_12x12_UNORM_BLOCK     = 183,
       VK_FORMAT_ASTC_12x12_SRGB_BLOCK      = 184,
       VK_FORMAT_BEGIN_RANGE                = VK_FORMAT_UNDEFINED,
       VK_FORMAT_END_RANGE                  = VK_FORMAT_ASTC_12x12_SRGB_BLOCK,
       VK_FORMAT_RANGE_SIZE                 = ( VK_FORMAT_ASTC_12x12_SRGB_BLOCK - VK_FORMAT_UNDEFINED + 1 ),
       VK_FORMAT_MAX_ENUM                   = $7FFFFFFF
     );

type P_VkImageType = ^T_VkImageType;
     T_VkImageType = (
       VK_IMAGE_TYPE_1D          = 0,
       VK_IMAGE_TYPE_2D          = 1,
       VK_IMAGE_TYPE_3D          = 2,
       VK_IMAGE_TYPE_BEGIN_RANGE = VK_IMAGE_TYPE_1D,
       VK_IMAGE_TYPE_END_RANGE   = VK_IMAGE_TYPE_3D,
       VK_IMAGE_TYPE_RANGE_SIZE  = ( VK_IMAGE_TYPE_3D - VK_IMAGE_TYPE_1D + 1 ),
       VK_IMAGE_TYPE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkImageTiling = ^T_VkImageTiling;
     T_VkImageTiling = (
       VK_IMAGE_TILING_OPTIMAL     = 0,
       VK_IMAGE_TILING_LINEAR      = 1,
       VK_IMAGE_TILING_BEGIN_RANGE = VK_IMAGE_TILING_OPTIMAL,
       VK_IMAGE_TILING_END_RANGE   = VK_IMAGE_TILING_LINEAR,
       VK_IMAGE_TILING_RANGE_SIZE  = ( VK_IMAGE_TILING_LINEAR - VK_IMAGE_TILING_OPTIMAL + 1 ),
       VK_IMAGE_TILING_MAX_ENUM    = $7FFFFFFF
     );

type P_VkPhysicalDeviceType = ^T_VkPhysicalDeviceType;
     T_VkPhysicalDeviceType = (
       VK_PHYSICAL_DEVICE_TYPE_OTHER          = 0,
       VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = 1,
       VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   = 2,
       VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    = 3,
       VK_PHYSICAL_DEVICE_TYPE_CPU            = 4,
       VK_PHYSICAL_DEVICE_TYPE_BEGIN_RANGE    = VK_PHYSICAL_DEVICE_TYPE_OTHER,
       VK_PHYSICAL_DEVICE_TYPE_END_RANGE      = VK_PHYSICAL_DEVICE_TYPE_CPU,
       VK_PHYSICAL_DEVICE_TYPE_RANGE_SIZE     = ( VK_PHYSICAL_DEVICE_TYPE_CPU - VK_PHYSICAL_DEVICE_TYPE_OTHER + 1 ),
       VK_PHYSICAL_DEVICE_TYPE_MAX_ENUM       = $7FFFFFFF
     );

type P_VkQueryType = ^T_VkQueryType;
     T_VkQueryType = (
       VK_QUERY_TYPE_OCCLUSION           = 0,
       VK_QUERY_TYPE_PIPELINE_STATISTICS = 1,
       VK_QUERY_TYPE_TIMESTAMP           = 2,
       VK_QUERY_TYPE_BEGIN_RANGE         = VK_QUERY_TYPE_OCCLUSION,
       VK_QUERY_TYPE_END_RANGE           = VK_QUERY_TYPE_TIMESTAMP,
       VK_QUERY_TYPE_RANGE_SIZE          = ( VK_QUERY_TYPE_TIMESTAMP - VK_QUERY_TYPE_OCCLUSION + 1 ),
       VK_QUERY_TYPE_MAX_ENUM            = $7FFFFFFF
     );

type P_VkSharingMode = ^T_VkSharingMode;
     T_VkSharingMode = (
       VK_SHARING_MODE_EXCLUSIVE   = 0,
       VK_SHARING_MODE_CONCURRENT  = 1,
       VK_SHARING_MODE_BEGIN_RANGE = VK_SHARING_MODE_EXCLUSIVE,
       VK_SHARING_MODE_END_RANGE   = VK_SHARING_MODE_CONCURRENT,
       VK_SHARING_MODE_RANGE_SIZE  = ( VK_SHARING_MODE_CONCURRENT - VK_SHARING_MODE_EXCLUSIVE + 1 ),
       VK_SHARING_MODE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkImageLayout = ^T_VkImageLayout;
     T_VkImageLayout = (
       VK_IMAGE_LAYOUT_UNDEFINED                        = 0,
       VK_IMAGE_LAYOUT_GENERAL                          = 1,
       VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL         = 2,
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = 3,
       VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL  = 4,
       VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL         = 5,
       VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL             = 6,
       VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL             = 7,
       VK_IMAGE_LAYOUT_PREINITIALIZED                   = 8,
       VK_IMAGE_LAYOUT_PRESENT_SRC_KHR                  = 1000001002,
       VK_IMAGE_LAYOUT_BEGIN_RANGE                      = VK_IMAGE_LAYOUT_UNDEFINED,
       VK_IMAGE_LAYOUT_END_RANGE                        = VK_IMAGE_LAYOUT_PREINITIALIZED,
       VK_IMAGE_LAYOUT_RANGE_SIZE                       = ( VK_IMAGE_LAYOUT_PREINITIALIZED - VK_IMAGE_LAYOUT_UNDEFINED + 1 ),
       VK_IMAGE_LAYOUT_MAX_ENUM                         = $7FFFFFFF
     );

type P_VkImageViewType = ^T_VkImageViewType;
     T_VkImageViewType = (
       VK_IMAGE_VIEW_TYPE_1D          = 0,
       VK_IMAGE_VIEW_TYPE_2D          = 1,
       VK_IMAGE_VIEW_TYPE_3D          = 2,
       VK_IMAGE_VIEW_TYPE_CUBE        = 3,
       VK_IMAGE_VIEW_TYPE_1D_ARRAY    = 4,
       VK_IMAGE_VIEW_TYPE_2D_ARRAY    = 5,
       VK_IMAGE_VIEW_TYPE_CUBE_ARRAY  = 6,
       VK_IMAGE_VIEW_TYPE_BEGIN_RANGE = VK_IMAGE_VIEW_TYPE_1D,
       VK_IMAGE_VIEW_TYPE_END_RANGE   = VK_IMAGE_VIEW_TYPE_CUBE_ARRAY,
       VK_IMAGE_VIEW_TYPE_RANGE_SIZE  = ( VK_IMAGE_VIEW_TYPE_CUBE_ARRAY - VK_IMAGE_VIEW_TYPE_1D + 1 ),
       VK_IMAGE_VIEW_TYPE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkComponentSwizzle = ^T_VkComponentSwizzle;
     T_VkComponentSwizzle = (
       VK_COMPONENT_SWIZZLE_IDENTITY    = 0,
       VK_COMPONENT_SWIZZLE_ZERO        = 1,
       VK_COMPONENT_SWIZZLE_ONE         = 2,
       VK_COMPONENT_SWIZZLE_R           = 3,
       VK_COMPONENT_SWIZZLE_G           = 4,
       VK_COMPONENT_SWIZZLE_B           = 5,
       VK_COMPONENT_SWIZZLE_A           = 6,
       VK_COMPONENT_SWIZZLE_BEGIN_RANGE = VK_COMPONENT_SWIZZLE_IDENTITY,
       VK_COMPONENT_SWIZZLE_END_RANGE   = VK_COMPONENT_SWIZZLE_A,
       VK_COMPONENT_SWIZZLE_RANGE_SIZE  = ( VK_COMPONENT_SWIZZLE_A - VK_COMPONENT_SWIZZLE_IDENTITY + 1 ),
       VK_COMPONENT_SWIZZLE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkVertexInputRate = ^T_VkVertexInputRate;
     T_VkVertexInputRate = (
       VK_VERTEX_INPUT_RATE_VERTEX      = 0,
       VK_VERTEX_INPUT_RATE_INSTANCE    = 1,
       VK_VERTEX_INPUT_RATE_BEGIN_RANGE = VK_VERTEX_INPUT_RATE_VERTEX,
       VK_VERTEX_INPUT_RATE_END_RANGE   = VK_VERTEX_INPUT_RATE_INSTANCE,
       VK_VERTEX_INPUT_RATE_RANGE_SIZE  = ( VK_VERTEX_INPUT_RATE_INSTANCE - VK_VERTEX_INPUT_RATE_VERTEX + 1 ),
       VK_VERTEX_INPUT_RATE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkPrimitiveTopology = ^T_VkPrimitiveTopology;
     T_VkPrimitiveTopology = (
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
       VK_PRIMITIVE_TOPOLOGY_BEGIN_RANGE                   = VK_PRIMITIVE_TOPOLOGY_POINT_LIST,
       VK_PRIMITIVE_TOPOLOGY_END_RANGE                     = VK_PRIMITIVE_TOPOLOGY_PATCH_LIST,
       VK_PRIMITIVE_TOPOLOGY_RANGE_SIZE                    = ( VK_PRIMITIVE_TOPOLOGY_PATCH_LIST - VK_PRIMITIVE_TOPOLOGY_POINT_LIST + 1 ),
       VK_PRIMITIVE_TOPOLOGY_MAX_ENUM                      = $7FFFFFFF
     );

type P_VkPolygonMode = ^T_VkPolygonMode;
     T_VkPolygonMode = (
       VK_POLYGON_MODE_FILL        = 0,
       VK_POLYGON_MODE_LINE        = 1,
       VK_POLYGON_MODE_POINT       = 2,
       VK_POLYGON_MODE_BEGIN_RANGE = VK_POLYGON_MODE_FILL,
       VK_POLYGON_MODE_END_RANGE   = VK_POLYGON_MODE_POINT,
       VK_POLYGON_MODE_RANGE_SIZE  = ( VK_POLYGON_MODE_POINT - VK_POLYGON_MODE_FILL + 1 ),
       VK_POLYGON_MODE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkFrontFace = ^T_VkFrontFace;
     T_VkFrontFace = (
       VK_FRONT_FACE_COUNTER_CLOCKWISE = 0,
       VK_FRONT_FACE_CLOCKWISE         = 1,
       VK_FRONT_FACE_BEGIN_RANGE       = VK_FRONT_FACE_COUNTER_CLOCKWISE,
       VK_FRONT_FACE_END_RANGE         = VK_FRONT_FACE_CLOCKWISE,
       VK_FRONT_FACE_RANGE_SIZE        = ( VK_FRONT_FACE_CLOCKWISE - VK_FRONT_FACE_COUNTER_CLOCKWISE + 1 ),
       VK_FRONT_FACE_MAX_ENUM          = $7FFFFFFF
     );

type P_VkCompareOp = ^T_VkCompareOp;
     T_VkCompareOp = (
       VK_COMPARE_OP_NEVER            = 0,
       VK_COMPARE_OP_LESS             = 1,
       VK_COMPARE_OP_EQUAL            = 2,
       VK_COMPARE_OP_LESS_OR_EQUAL    = 3,
       VK_COMPARE_OP_GREATER          = 4,
       VK_COMPARE_OP_NOT_EQUAL        = 5,
       VK_COMPARE_OP_GREATER_OR_EQUAL = 6,
       VK_COMPARE_OP_ALWAYS           = 7,
       VK_COMPARE_OP_BEGIN_RANGE      = VK_COMPARE_OP_NEVER,
       VK_COMPARE_OP_END_RANGE        = VK_COMPARE_OP_ALWAYS,
       VK_COMPARE_OP_RANGE_SIZE       = ( VK_COMPARE_OP_ALWAYS - VK_COMPARE_OP_NEVER + 1 ),
       VK_COMPARE_OP_MAX_ENUM         = $7FFFFFFF
     );

type P_VkStencilOp = ^T_VkStencilOp;
     T_VkStencilOp = (
       VK_STENCIL_OP_KEEP = 0,
       VK_STENCIL_OP_ZERO = 1,
       VK_STENCIL_OP_REPLACE = 2,
       VK_STENCIL_OP_INCREMENT_AND_CLAMP = 3,
       VK_STENCIL_OP_DECREMENT_AND_CLAMP = 4,
       VK_STENCIL_OP_INVERT = 5,
       VK_STENCIL_OP_INCREMENT_AND_WRAP = 6,
       VK_STENCIL_OP_DECREMENT_AND_WRAP = 7,
       VK_STENCIL_OP_BEGIN_RANGE = VK_STENCIL_OP_KEEP,
       VK_STENCIL_OP_END_RANGE = VK_STENCIL_OP_DECREMENT_AND_WRAP,
       VK_STENCIL_OP_RANGE_SIZE = ( VK_STENCIL_OP_DECREMENT_AND_WRAP - VK_STENCIL_OP_KEEP + 1 ),
       VK_STENCIL_OP_MAX_ENUM = $7FFFFFFF
     );

type P_VkLogicOp = ^T_VkLogicOp;
     T_VkLogicOp = (
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
       VK_LOGIC_OP_BEGIN_RANGE   = VK_LOGIC_OP_CLEAR,
       VK_LOGIC_OP_END_RANGE     = VK_LOGIC_OP_SET,
       VK_LOGIC_OP_RANGE_SIZE    = ( VK_LOGIC_OP_SET - VK_LOGIC_OP_CLEAR + 1 ),
       VK_LOGIC_OP_MAX_ENUM      = $7FFFFFFF
     );

type P_VkBlendFactor = ^T_VkBlendFactor;
     T_VkBlendFactor = (
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
       VK_BLEND_FACTOR_BEGIN_RANGE              = VK_BLEND_FACTOR_ZERO,
       VK_BLEND_FACTOR_END_RANGE                = VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA,
       VK_BLEND_FACTOR_RANGE_SIZE               = ( VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA - VK_BLEND_FACTOR_ZERO + 1 ),
       VK_BLEND_FACTOR_MAX_ENUM                 = $7FFFFFFF
     );

type P_VkBlendOp = ^T_VkBlendOp;
     T_VkBlendOp = (
       VK_BLEND_OP_ADD              = 0,
       VK_BLEND_OP_SUBTRACT         = 1,
       VK_BLEND_OP_REVERSE_SUBTRACT = 2,
       VK_BLEND_OP_MIN              = 3,
       VK_BLEND_OP_MAX              = 4,
       VK_BLEND_OP_BEGIN_RANGE      = VK_BLEND_OP_ADD,
       VK_BLEND_OP_END_RANGE        = VK_BLEND_OP_MAX,
       VK_BLEND_OP_RANGE_SIZE       = ( VK_BLEND_OP_MAX - VK_BLEND_OP_ADD + 1 ),
       VK_BLEND_OP_MAX_ENUM         = $7FFFFFFF
     );

type P_VkDynamicState = ^T_VkDynamicState;
     T_VkDynamicState = (
       VK_DYNAMIC_STATE_VIEWPORT             = 0,
       VK_DYNAMIC_STATE_SCISSOR              = 1,
       VK_DYNAMIC_STATE_LINE_WIDTH           = 2,
       VK_DYNAMIC_STATE_DEPTH_BIAS           = 3,
       VK_DYNAMIC_STATE_BLEND_CONSTANTS      = 4,
       VK_DYNAMIC_STATE_DEPTH_BOUNDS         = 5,
       VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = 6,
       VK_DYNAMIC_STATE_STENCIL_WRITE_MASK   = 7,
       VK_DYNAMIC_STATE_STENCIL_REFERENCE    = 8,
       VK_DYNAMIC_STATE_BEGIN_RANGE          = VK_DYNAMIC_STATE_VIEWPORT,
       VK_DYNAMIC_STATE_END_RANGE            = VK_DYNAMIC_STATE_STENCIL_REFERENCE,
       VK_DYNAMIC_STATE_RANGE_SIZE           = ( VK_DYNAMIC_STATE_STENCIL_REFERENCE - VK_DYNAMIC_STATE_VIEWPORT + 1 ),
       VK_DYNAMIC_STATE_MAX_ENUM             = $7FFFFFFF
     );

type P_VkFilter = ^T_VkFilter;
     T_VkFilter = (
       VK_FILTER_NEAREST     = 0,
       VK_FILTER_LINEAR      = 1,
       VK_FILTER_BEGIN_RANGE = VK_FILTER_NEAREST,
       VK_FILTER_END_RANGE   = VK_FILTER_LINEAR,
       VK_FILTER_RANGE_SIZE  = ( VK_FILTER_LINEAR - VK_FILTER_NEAREST + 1 ),
       VK_FILTER_MAX_ENUM    = $7FFFFFFF
     );

type P_VkSamplerMipmapMode = ^T_VkSamplerMipmapMode;
     T_VkSamplerMipmapMode = (
       VK_SAMPLER_MIPMAP_MODE_NEAREST     = 0,
       VK_SAMPLER_MIPMAP_MODE_LINEAR      = 1,
       VK_SAMPLER_MIPMAP_MODE_BEGIN_RANGE = VK_SAMPLER_MIPMAP_MODE_NEAREST,
       VK_SAMPLER_MIPMAP_MODE_END_RANGE   = VK_SAMPLER_MIPMAP_MODE_LINEAR,
       VK_SAMPLER_MIPMAP_MODE_RANGE_SIZE  = ( VK_SAMPLER_MIPMAP_MODE_LINEAR - VK_SAMPLER_MIPMAP_MODE_NEAREST + 1 ),
       VK_SAMPLER_MIPMAP_MODE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkSamplerAddressMode = ^T_VkSamplerAddressMode;
     T_VkSamplerAddressMode = (
       VK_SAMPLER_ADDRESS_MODE_REPEAT               = 0,
       VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT      = 1,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE        = 2,
       VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER      = 3,
       VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = 4,
       VK_SAMPLER_ADDRESS_MODE_BEGIN_RANGE          = VK_SAMPLER_ADDRESS_MODE_REPEAT,
       VK_SAMPLER_ADDRESS_MODE_END_RANGE            = VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE,
       VK_SAMPLER_ADDRESS_MODE_RANGE_SIZE           = ( VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE - VK_SAMPLER_ADDRESS_MODE_REPEAT + 1 ),
       VK_SAMPLER_ADDRESS_MODE_MAX_ENUM             = $7FFFFFFF
     );

type P_VkBorderColor = ^T_VkBorderColor;
     T_VkBorderColor = (
       VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = 0,
       VK_BORDER_COLOR_INT_TRANSPARENT_BLACK   = 1,
       VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK      = 2,
       VK_BORDER_COLOR_INT_OPAQUE_BLACK        = 3,
       VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE      = 4,
       VK_BORDER_COLOR_INT_OPAQUE_WHITE        = 5,
       VK_BORDER_COLOR_BEGIN_RANGE             = VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
       VK_BORDER_COLOR_END_RANGE               = VK_BORDER_COLOR_INT_OPAQUE_WHITE,
       VK_BORDER_COLOR_RANGE_SIZE              = ( VK_BORDER_COLOR_INT_OPAQUE_WHITE - VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK + 1 ),
       VK_BORDER_COLOR_MAX_ENUM                = $7FFFFFFF
     );

type P_VkDescriptorType = ^T_VkDescriptorType;
     T_VkDescriptorType = (
       VK_DESCRIPTOR_TYPE_SAMPLER                = 0,
       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = 1,
       VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE          = 2,
       VK_DESCRIPTOR_TYPE_STORAGE_IMAGE          = 3,
       VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER   = 4,
       VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER   = 5,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER         = 6,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER         = 7,
       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = 8,
       VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = 9,
       VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT       = 10,
       VK_DESCRIPTOR_TYPE_BEGIN_RANGE            = VK_DESCRIPTOR_TYPE_SAMPLER,
       VK_DESCRIPTOR_TYPE_END_RANGE              = VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
       VK_DESCRIPTOR_TYPE_RANGE_SIZE             = ( VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT - VK_DESCRIPTOR_TYPE_SAMPLER + 1 ),
       VK_DESCRIPTOR_TYPE_MAX_ENUM               = $7FFFFFFF
     );

type P_VkAttachmentLoadOp = ^T_VkAttachmentLoadOp;
     T_VkAttachmentLoadOp = (
       VK_ATTACHMENT_LOAD_OP_LOAD        = 0,
       VK_ATTACHMENT_LOAD_OP_CLEAR       = 1,
       VK_ATTACHMENT_LOAD_OP_DONT_CARE   = 2,
       VK_ATTACHMENT_LOAD_OP_BEGIN_RANGE = VK_ATTACHMENT_LOAD_OP_LOAD,
       VK_ATTACHMENT_LOAD_OP_END_RANGE   = VK_ATTACHMENT_LOAD_OP_DONT_CARE,
       VK_ATTACHMENT_LOAD_OP_RANGE_SIZE  = ( VK_ATTACHMENT_LOAD_OP_DONT_CARE - VK_ATTACHMENT_LOAD_OP_LOAD + 1 ),
       VK_ATTACHMENT_LOAD_OP_MAX_ENUM    = $7FFFFFFF
     );

type P_VkAttachmentStoreOp = ^T_VkAttachmentStoreOp;
     T_VkAttachmentStoreOp = (
       VK_ATTACHMENT_STORE_OP_STORE       = 0,
       VK_ATTACHMENT_STORE_OP_DONT_CARE   = 1,
       VK_ATTACHMENT_STORE_OP_BEGIN_RANGE = VK_ATTACHMENT_STORE_OP_STORE,
       VK_ATTACHMENT_STORE_OP_END_RANGE   = VK_ATTACHMENT_STORE_OP_DONT_CARE,
       VK_ATTACHMENT_STORE_OP_RANGE_SIZE  = ( VK_ATTACHMENT_STORE_OP_DONT_CARE - VK_ATTACHMENT_STORE_OP_STORE + 1 ),
       VK_ATTACHMENT_STORE_OP_MAX_ENUM    = $7FFFFFFF
     );

type P_VkPipelineBindPoint = ^T_VkPipelineBindPoint;
     T_VkPipelineBindPoint = (
       VK_PIPELINE_BIND_POINT_GRAPHICS    = 0,
       VK_PIPELINE_BIND_POINT_COMPUTE     = 1,
       VK_PIPELINE_BIND_POINT_BEGIN_RANGE = VK_PIPELINE_BIND_POINT_GRAPHICS,
       VK_PIPELINE_BIND_POINT_END_RANGE   = VK_PIPELINE_BIND_POINT_COMPUTE,
       VK_PIPELINE_BIND_POINT_RANGE_SIZE  = ( VK_PIPELINE_BIND_POINT_COMPUTE - VK_PIPELINE_BIND_POINT_GRAPHICS + 1 ),
       VK_PIPELINE_BIND_POINT_MAX_ENUM    = $7FFFFFFF
     );

type P_VkCommandBufferLevel = ^T_VkCommandBufferLevel;
     T_VkCommandBufferLevel = (
       VK_COMMAND_BUFFER_LEVEL_PRIMARY     = 0,
       VK_COMMAND_BUFFER_LEVEL_SECONDARY   = 1,
       VK_COMMAND_BUFFER_LEVEL_BEGIN_RANGE = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
       VK_COMMAND_BUFFER_LEVEL_END_RANGE   = VK_COMMAND_BUFFER_LEVEL_SECONDARY,
       VK_COMMAND_BUFFER_LEVEL_RANGE_SIZE  = ( VK_COMMAND_BUFFER_LEVEL_SECONDARY - VK_COMMAND_BUFFER_LEVEL_PRIMARY + 1 ),
       VK_COMMAND_BUFFER_LEVEL_MAX_ENUM    = $7FFFFFFF
     );

type P_VkIndexType = ^T_VkIndexType;
     T_VkIndexType = (
       VK_INDEX_TYPE_UINT16      = 0,
       VK_INDEX_TYPE_UINT32      = 1,
       VK_INDEX_TYPE_BEGIN_RANGE = VK_INDEX_TYPE_UINT16,
       VK_INDEX_TYPE_END_RANGE   = VK_INDEX_TYPE_UINT32,
       VK_INDEX_TYPE_RANGE_SIZE  = ( VK_INDEX_TYPE_UINT32 - VK_INDEX_TYPE_UINT16 + 1 ),
       VK_INDEX_TYPE_MAX_ENUM    = $7FFFFFFF
     );

type P_VkSubpassContents = ^T_VkSubpassContents;
     T_VkSubpassContents = (
       VK_SUBPASS_CONTENTS_INLINE                    = 0,
       VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = 1,
       VK_SUBPASS_CONTENTS_BEGIN_RANGE               = VK_SUBPASS_CONTENTS_INLINE,
       VK_SUBPASS_CONTENTS_END_RANGE                 = VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS,
       VK_SUBPASS_CONTENTS_RANGE_SIZE                = ( VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS - VK_SUBPASS_CONTENTS_INLINE + 1 ),
       VK_SUBPASS_CONTENTS_MAX_ENUM                  = $7FFFFFFF
     );

type T_VkInstanceCreateFlags = T_VkFlags;

type P_VkFormatFeatureFlagBits = ^T_VkFormatFeatureFlagBits;
     T_VkFormatFeatureFlagBits = (
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT               = $00000001,
       VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT               = $00000002,
       VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT        = $00000004,
       VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT        = $00000008,
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT        = $00000010,
       VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = $00000020,
       VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT               = $00000040,
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT            = $00000080,
       VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT      = $00000100,
       VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT    = $00000200,
       VK_FORMAT_FEATURE_BLIT_SRC_BIT                    = $00000400,
       VK_FORMAT_FEATURE_BLIT_DST_BIT                    = $00000800,
       VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = $00001000
     );
type T_VkFormatFeatureFlags = T_VkFlags;

type P_VkImageUsageFlagBits = ^T_VkImageUsageFlagBits;
     T_VkImageUsageFlagBits = (
       VK_IMAGE_USAGE_TRANSFER_SRC_BIT             = $00000001,
       VK_IMAGE_USAGE_TRANSFER_DST_BIT             = $00000002,
       VK_IMAGE_USAGE_SAMPLED_BIT                  = $00000004,
       VK_IMAGE_USAGE_STORAGE_BIT                  = $00000008,
       VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT         = $00000010,
       VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = $00000020,
       VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT     = $00000040,
       VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT         = $00000080
     );
type T_VkImageUsageFlags = T_VkFlags;

type P_VkImageCreateFlagBits = ^T_VkImageCreateFlagBits;
     T_VkImageCreateFlagBits = (
       VK_IMAGE_CREATE_SPARSE_BINDING_BIT   = $00000001,
       VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = $00000002,
       VK_IMAGE_CREATE_SPARSE_ALIASED_BIT   = $00000004,
       VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT   = $00000008,
       VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT  = $00000010
     );
type T_VkImageCreateFlags = T_VkFlags;

type P_VkSampleCountFlagBits = ^T_VkSampleCountFlagBits;
     T_VkSampleCountFlagBits = (
       VK_SAMPLE_COUNT_1_BIT  = $00000001,
       VK_SAMPLE_COUNT_2_BIT  = $00000002,
       VK_SAMPLE_COUNT_4_BIT  = $00000004,
       VK_SAMPLE_COUNT_8_BIT  = $00000008,
       VK_SAMPLE_COUNT_16_BIT = $00000010,
       VK_SAMPLE_COUNT_32_BIT = $00000020,
       VK_SAMPLE_COUNT_64_BIT = $00000040
     );
type T_VkSampleCountFlags = T_VkFlags;

type P_VkQueueFlagBits = ^T_VkQueueFlagBits;
     T_VkQueueFlagBits = (
       VK_QUEUE_GRAPHICS_BIT       = $00000001,
       VK_QUEUE_COMPUTE_BIT        = $00000002,
       VK_QUEUE_TRANSFER_BIT       = $00000004,
       VK_QUEUE_SPARSE_BINDING_BIT = $00000008
     );
type T_VkQueueFlags = T_VkFlags;

type P_VkMemoryPropertyFlagBits = ^T_VkMemoryPropertyFlagBits;
     T_VkMemoryPropertyFlagBits = (
       VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT     = $00000001,
       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT     = $00000002,
       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT    = $00000004,
       VK_MEMORY_PROPERTY_HOST_CACHED_BIT      = $00000008,
       VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = $00000010
     );
type T_VkMemoryPropertyFlags = T_VkFlags;

type P_VkMemoryHeapFlagBits = ^T_VkMemoryHeapFlagBits;
     T_VkMemoryHeapFlagBits = (
       VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = $00000001
     );
type T_VkMemoryHeapFlags = T_VkFlags;
type T_VkDeviceCreateFlags = T_VkFlags;
type T_VkDeviceQueueCreateFlags = T_VkFlags;

type P_VkPipelineStageFlagBits = ^T_VkPipelineStageFlagBits;
     T_VkPipelineStageFlagBits = (
       VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT                    = $00000001,
       VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT                  = $00000002,
       VK_PIPELINE_STAGE_VERTEX_INPUT_BIT                   = $00000004,
       VK_PIPELINE_STAGE_VERTEX_SHADER_BIT                  = $00000008,
       VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT    = $00000010,
       VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = $00000020,
       VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT                = $00000040,
       VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT                = $00000080,
       VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT           = $00000100,
       VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT            = $00000200,
       VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT        = $00000400,
       VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT                 = $00000800,
       VK_PIPELINE_STAGE_TRANSFER_BIT                       = $00001000,
       VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT                 = $00002000,
       VK_PIPELINE_STAGE_HOST_BIT                           = $00004000,
       VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT                   = $00008000,
       VK_PIPELINE_STAGE_ALL_COMMANDS_BIT                   = $00010000
     );
type P_VkPipelineStageFlags = ^T_VkPipelineStageFlags;
     T_VkPipelineStageFlags = T_VkFlags;
type T_VkMemoryMapFlags = T_VkFlags;

type P_VkImageAspectFlagBits = ^T_VkImageAspectFlagBits;
     T_VkImageAspectFlagBits = (
       VK_IMAGE_ASPECT_COLOR_BIT    = $00000001,
       VK_IMAGE_ASPECT_DEPTH_BIT    = $00000002,
       VK_IMAGE_ASPECT_STENCIL_BIT  = $00000004,
       VK_IMAGE_ASPECT_METADATA_BIT = $00000008
     );
type T_VkImageAspectFlags = T_VkFlags;

type P_VkSparseImageFormatFlagBits = ^T_VkSparseImageFormatFlagBits;
     T_VkSparseImageFormatFlagBits = (
       VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT         = $00000001,
       VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT       = $00000002,
       VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = $00000004
     );
type T_VkSparseImageFormatFlags = T_VkFlags;

type P_VkSparseMemoryBindFlagBits = ^T_VkSparseMemoryBindFlagBits;
     T_VkSparseMemoryBindFlagBits = (
       VK_SPARSE_MEMORY_BIND_METADATA_BIT = $00000001
     );
type T_VkSparseMemoryBindFlags = T_VkFlags;

type P_VkFenceCreateFlagBits = ^T_VkFenceCreateFlagBits;
     T_VkFenceCreateFlagBits = (
       VK_FENCE_CREATE_SIGNALED_BIT = $00000001
     );
type T_VkFenceCreateFlags     = T_VkFlags;
type T_VkSemaphoreCreateFlags = T_VkFlags;
type T_VkEventCreateFlags     = T_VkFlags;
type T_VkQueryPoolCreateFlags = T_VkFlags;

type P_VkQueryPipelineStatisticFlagBits = ^T_VkQueryPipelineStatisticFlagBits;
     T_VkQueryPipelineStatisticFlagBits = (
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
       VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT                 = $00000400
     );
type T_VkQueryPipelineStatisticFlags = T_VkFlags;

type P_VkQueryResultFlagBits = ^T_VkQueryResultFlagBits;
     T_VkQueryResultFlagBits = (
       VK_QUERY_RESULT_64_BIT                = $00000001,
       VK_QUERY_RESULT_WAIT_BIT              = $00000002,
       VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = $00000004,
       VK_QUERY_RESULT_PARTIAL_BIT           = $00000008
     );
type T_VkQueryResultFlags = T_VkFlags;

type P_VkBufferCreateFlagBits = ^T_VkBufferCreateFlagBits;
     T_VkBufferCreateFlagBits = (
       VK_BUFFER_CREATE_SPARSE_BINDING_BIT   = $00000001,
       VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = $00000002,
       VK_BUFFER_CREATE_SPARSE_ALIASED_BIT   = $00000004
     );
type T_VkBufferCreateFlags = T_VkFlags;

type P_VkBufferUsageFlagBits = ^T_VkBufferUsageFlagBits;
     T_VkBufferUsageFlagBits = (
       VK_BUFFER_USAGE_TRANSFER_SRC_BIT         = $00000001,
       VK_BUFFER_USAGE_TRANSFER_DST_BIT         = $00000002,
       VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = $00000004,
       VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = $00000008,
       VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT       = $00000010,
       VK_BUFFER_USAGE_STORAGE_BUFFER_BIT       = $00000020,
       VK_BUFFER_USAGE_INDEX_BUFFER_BIT         = $00000040,
       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT        = $00000080,
       VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT      = $00000100
     );
type T_VkBufferUsageFlags         = T_VkFlags;
type T_VkBufferViewCreateFlags    = T_VkFlags;
type T_VkImageViewCreateFlags     = T_VkFlags;
type T_VkShaderModuleCreateFlags  = T_VkFlags;
type T_VkPipelineCacheCreateFlags = T_VkFlags;

type P_VkPipelineCreateFlagBits = ^T_VkPipelineCreateFlagBits;
     T_VkPipelineCreateFlagBits = (
       VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = $00000001,
       VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT    = $00000002,
       VK_PIPELINE_CREATE_DERIVATIVE_BIT           = $00000004
     );
type T_VkPipelineCreateFlags = T_VkFlags;
type T_VkPipelineShaderStageCreateFlags = T_VkFlags;

type P_VkShaderStageFlagBits = ^T_VkShaderStageFlagBits;
     T_VkShaderStageFlagBits = (
       VK_SHADER_STAGE_VERTEX_BIT                  = $00000001,
       VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT    = $00000002,
       VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = $00000004,
       VK_SHADER_STAGE_GEOMETRY_BIT                = $00000008,
       VK_SHADER_STAGE_FRAGMENT_BIT                = $00000010,
       VK_SHADER_STAGE_COMPUTE_BIT                 = $00000020,
       VK_SHADER_STAGE_ALL_GRAPHICS                = $1F,
       VK_SHADER_STAGE_ALL                         = $7FFFFFFF
     );
type T_VkPipelineVertexInputStateCreateFlags   = T_VkFlags;
type T_VkPipelineInputAssemblyStateCreateFlags = T_VkFlags;
type T_VkPipelineTessellationStateCreateFlags  = T_VkFlags;
type T_VkPipelineViewportStateCreateFlags      = T_VkFlags;
type T_VkPipelineRasterizationStateCreateFlags = T_VkFlags;

type P_VkCullModeFlagBits = ^T_VkCullModeFlagBits;
     T_VkCullModeFlagBits = (
       VK_CULL_MODE_NONE           = 0,
       VK_CULL_MODE_FRONT_BIT      = $00000001,
       VK_CULL_MODE_BACK_BIT       = $00000002,
       VK_CULL_MODE_FRONT_AND_BACK = $3
     );
type T_VkCullModeFlags                        = T_VkFlags;
type T_VkPipelineMultisampleStateCreateFlags  = T_VkFlags;
type T_VkPipelineDepthStencilStateCreateFlags = T_VkFlags;
type T_VkPipelineColorBlendStateCreateFlags   = T_VkFlags;

type P_VkColorComponentFlagBits = ^T_VkColorComponentFlagBits;
     T_VkColorComponentFlagBits = (
       VK_COLOR_COMPONENT_R_BIT = $00000001,
       VK_COLOR_COMPONENT_G_BIT = $00000002,
       VK_COLOR_COMPONENT_B_BIT = $00000004,
       VK_COLOR_COMPONENT_A_BIT = $00000008
     );
type T_VkColorComponentFlags             = T_VkFlags;
type T_VkPipelineDynamicStateCreateFlags = T_VkFlags;
type T_VkPipelineLayoutCreateFlags       = T_VkFlags;
type T_VkShaderStageFlags                = T_VkFlags;
type T_VkSamplerCreateFlags              = T_VkFlags;
type T_VkDescriptorSetLayoutCreateFlags  = T_VkFlags;

type P_VkDescriptorPoolCreateFlagBits = ^T_VkDescriptorPoolCreateFlagBits;
     T_VkDescriptorPoolCreateFlagBits = (
       VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = $00000001
     );
type T_VkDescriptorPoolCreateFlags = T_VkFlags;
type T_VkDescriptorPoolResetFlags  = T_VkFlags;
type T_VkFramebufferCreateFlags    = T_VkFlags;
type T_VkRenderPassCreateFlags     = T_VkFlags;

type P_VkAttachmentDescriptionFlagBits = ^T_VkAttachmentDescriptionFlagBits;
     T_VkAttachmentDescriptionFlagBits = (
       VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = $00000001
     );
type T_VkAttachmentDescriptionFlags = T_VkFlags;
type T_VkSubpassDescriptionFlags = T_VkFlags;

type P_VkAccessFlagBits = ^T_VkAccessFlagBits;
     T_VkAccessFlagBits = (
       VK_ACCESS_INDIRECT_COMMAND_READ_BIT          = $00000001,
       VK_ACCESS_INDEX_READ_BIT                     = $00000002,
       VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT          = $00000004,
       VK_ACCESS_UNIFORM_READ_BIT                   = $00000008,
       VK_ACCESS_INPUT_ATTACHMENT_READ_BIT          = $00000010,
       VK_ACCESS_SHADER_READ_BIT                    = $00000020,
       VK_ACCESS_SHADER_WRITE_BIT                   = $00000040,
       VK_ACCESS_COLOR_ATTACHMENT_READ_BIT          = $00000080,
       VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT         = $00000100,
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT  = $00000200,
       VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = $00000400,
       VK_ACCESS_TRANSFER_READ_BIT                  = $00000800,
       VK_ACCESS_TRANSFER_WRITE_BIT                 = $00001000,
       VK_ACCESS_HOST_READ_BIT                      = $00002000,
       VK_ACCESS_HOST_WRITE_BIT                     = $00004000,
       VK_ACCESS_MEMORY_READ_BIT                    = $00008000,
       VK_ACCESS_MEMORY_WRITE_BIT                   = $00010000
     );
type T_VkAccessFlags = T_VkFlags;

type P_VkDependencyFlagBits = ^T_VkDependencyFlagBits;
     T_VkDependencyFlagBits = (
       VK_DEPENDENCY_BY_REGION_BIT = $00000001
     );
type T_VkDependencyFlags = T_VkFlags;

type P_VkCommandPoolCreateFlagBits = ^T_VkCommandPoolCreateFlagBits;
     T_VkCommandPoolCreateFlagBits = (
       VK_COMMAND_POOL_CREATE_TRANSIENT_BIT            = $00000001,
       VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = $00000002
     );
type T_VkCommandPoolCreateFlags = T_VkFlags;

type P_VkCommandPoolResetFlagBits = ^T_VkCommandPoolResetFlagBits;
     T_VkCommandPoolResetFlagBits = (
       VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = $00000001
     );
type T_VkCommandPoolResetFlags = T_VkFlags;

type P_VkCommandBufferUsageFlagBits = ^T_VkCommandBufferUsageFlagBits;
     T_VkCommandBufferUsageFlagBits = (
       VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT      = $00000001,
       VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = $00000002,
       VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT     = $00000004
     );
type T_VkCommandBufferUsageFlags = T_VkFlags;

type P_VkQueryControlFlagBits = ^T_VkQueryControlFlagBits;
     T_VkQueryControlFlagBits = (
       VK_QUERY_CONTROL_PRECISE_BIT = $00000001
     );
type T_VkQueryControlFlags = T_VkFlags;

type P_VkCommandBufferResetFlagBits = ^T_VkCommandBufferResetFlagBits;
     T_VkCommandBufferResetFlagBits = (
       VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = $00000001
     );
type T_VkCommandBufferResetFlags = T_VkFlags;

type P_VkStencilFaceFlagBits = ^T_VkStencilFaceFlagBits;
     T_VkStencilFaceFlagBits = (
       VK_STENCIL_FACE_FRONT_BIT = $00000001,
       VK_STENCIL_FACE_BACK_BIT  = $00000002,
       VK_STENCIL_FRONT_AND_BACK = $3
     );
type T_VkStencilFaceFlags = T_VkFlags;

type PFN_vkAllocationFunction = function(
         pUserData_        :P_void;
          size_            :T_size_t;
          alignment_       :T_size_t;
          allocationScope_ :T_VkSystemAllocationScope ) :P_void; stdcall;

type PFN_vkReallocationFunction = function(
         pUserData_        :P_void;
         pOriginal_        :P_void;
          size_            :T_size_t;
          alignment_       :T_size_t;
          allocationScope_ :T_VkSystemAllocationScope ) :P_void; stdcall;

type PFN_vkFreeFunction = procedure(
         pUserData_  :P_void;
         pMemory_    :P_void ); stdcall;

type PFN_vkInternalAllocationNotification = procedure(
         pUserData_        :P_void;
          size_            :T_size_t;
          allocationType_  :T_VkInternalAllocationType;
          allocationScope_ :T_VkSystemAllocationScope  ); stdcall;

type PFN_vkInternalFreeNotification = procedure(
         pUserData_        :P_void;
          size_            :T_size_t;
          allocationType_  :T_VkInternalAllocationType;
          allocationScope_ :T_VkSystemAllocationScope  ); stdcall;

type PFN_vkVoidFunction = procedure; stdcall;

type P_VkApplicationInfo = ^T_VkApplicationInfo;
     T_VkApplicationInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
         pApplicationName    :P_char;
          applicationVersion :T_uint32_t;
         pEngineName         :P_char;
          engineVersion      :T_uint32_t;
          apiVersion         :T_uint32_t;
     end;

type P_VkInstanceCreateInfo = ^T_VkInstanceCreateInfo;
     T_VkInstanceCreateInfo = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkInstanceCreateFlags;
         pApplicationInfo       :P_VkApplicationInfo;
          enabledLayerCount     :T_uint32_t;
        ppEnabledLayerNames     :PP_char;
          enabledExtensionCount :T_uint32_t;
        ppEnabledExtensionNames :PP_char;
     end;

type P_VkAllocationCallbacks = ^T_VkAllocationCallbacks;
     T_VkAllocationCallbacks = record
         pUserData           :P_void;
       pfnAllocation         :PFN_vkAllocationFunction;
       pfnReallocation       :PFN_vkReallocationFunction;
       pfnFree               :PFN_vkFreeFunction;
       pfnInternalAllocation :PFN_vkInternalAllocationNotification;
       pfnInternalFree       :PFN_vkInternalFreeNotification;
     end;

type P_VkPhysicalDeviceFeatures = ^T_VkPhysicalDeviceFeatures;
     T_VkPhysicalDeviceFeatures = record
          robustBufferAccess                      :T_VkBool32;
          fullDrawIndexUint32                     :T_VkBool32;
          imageCubeArray                          :T_VkBool32;
          independentBlend                        :T_VkBool32;
          geometryShader                          :T_VkBool32;
          tessellationShader                      :T_VkBool32;
          sampleRateShading                       :T_VkBool32;
          dualSrcBlend                            :T_VkBool32;
          logicOp                                 :T_VkBool32;
          multiDrawIndirect                       :T_VkBool32;
          drawIndirectFirstInstance               :T_VkBool32;
          depthClamp                              :T_VkBool32;
          depthBiasClamp                          :T_VkBool32;
          fillModeNonSolid                        :T_VkBool32;
          depthBounds                             :T_VkBool32;
          wideLines                               :T_VkBool32;
          largePoints                             :T_VkBool32;
          alphaToOne                              :T_VkBool32;
          multiViewport                           :T_VkBool32;
          samplerAnisotropy                       :T_VkBool32;
          textureCompressionETC2                  :T_VkBool32;
          textureCompressionASTC_LDR              :T_VkBool32;
          textureCompressionBC                    :T_VkBool32;
          occlusionQueryPrecise                   :T_VkBool32;
          pipelineStatisticsQuery                 :T_VkBool32;
          vertexPipelineStoresAndAtomics          :T_VkBool32;
          fragmentStoresAndAtomics                :T_VkBool32;
          shaderTessellationAndGeometryPointSize  :T_VkBool32;
          shaderImageGatherExtended               :T_VkBool32;
          shaderStorageImageExtendedFormats       :T_VkBool32;
          shaderStorageImageMultisample           :T_VkBool32;
          shaderStorageImageReadWithoutFormat     :T_VkBool32;
          shaderStorageImageWriteWithoutFormat    :T_VkBool32;
          shaderUniformBufferArrayDynamicIndexing :T_VkBool32;
          shaderSampledImageArrayDynamicIndexing  :T_VkBool32;
          shaderStorageBufferArrayDynamicIndexing :T_VkBool32;
          shaderStorageImageArrayDynamicIndexing  :T_VkBool32;
          shaderClipDistance                      :T_VkBool32;
          shaderCullDistance                      :T_VkBool32;
          shaderFloat64                           :T_VkBool32;
          shaderInt64                             :T_VkBool32;
          shaderInt16                             :T_VkBool32;
          shaderResourceResidency                 :T_VkBool32;
          shaderResourceMinLod                    :T_VkBool32;
          sparseBinding                           :T_VkBool32;
          sparseResidencyBuffer                   :T_VkBool32;
          sparseResidencyImage2D                  :T_VkBool32;
          sparseResidencyImage3D                  :T_VkBool32;
          sparseResidency2Samples                 :T_VkBool32;
          sparseResidency4Samples                 :T_VkBool32;
          sparseResidency8Samples                 :T_VkBool32;
          sparseResidency16Samples                :T_VkBool32;
          sparseResidencyAliased                  :T_VkBool32;
          variableMultisampleRate                 :T_VkBool32;
          inheritedQueries                        :T_VkBool32;
     end;

type P_VkFormatProperties = ^T_VkFormatProperties;
     T_VkFormatProperties = record
          linearTilingFeatures  :T_VkFormatFeatureFlags;
          optimalTilingFeatures :T_VkFormatFeatureFlags;
          bufferFeatures        :T_VkFormatFeatureFlags;
     end;

type P_VkExtent3D = ^T_VkExtent3D;
     T_VkExtent3D = record
          width  :T_uint32_t;
          height :T_uint32_t;
          depth  :T_uint32_t;
     end;

type P_VkImageFormatProperties = ^T_VkImageFormatProperties;
     T_VkImageFormatProperties = record
          maxExtent       :T_VkExtent3D;
          maxMipLevels    :T_uint32_t;
          maxArrayLayers  :T_uint32_t;
          sampleCounts    :T_VkSampleCountFlags;
          maxResourceSize :T_VkDeviceSize;
     end;

type P_VkPhysicalDeviceLimits = ^T_VkPhysicalDeviceLimits;
     T_VkPhysicalDeviceLimits = record
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
          bufferImageGranularity                          :T_VkDeviceSize;
          sparseAddressSpaceSize                          :T_VkDeviceSize;
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
          maxComputeWorkGroupCount                        :array [ 0..2 ] of T_uint32_t;
          maxComputeWorkGroupInvocations                  :T_uint32_t;
          maxComputeWorkGroupSize                         :array [ 0..2 ] of T_uint32_t;
          subPixelPrecisionBits                           :T_uint32_t;
          subTexelPrecisionBits                           :T_uint32_t;
          mipmapPrecisionBits                             :T_uint32_t;
          maxDrawIndexedIndexValue                        :T_uint32_t;
          maxDrawIndirectCount                            :T_uint32_t;
          maxSamplerLodBias                               :T_float;
          maxSamplerAnisotropy                            :T_float;
          maxViewports                                    :T_uint32_t;
          maxViewportDimensions                           :array [ 0..1 ] of T_uint32_t;
          viewportBoundsRange                             :array [ 0..1 ] of T_float;
          viewportSubPixelBits                            :T_uint32_t;
          minMemoryMapAlignment                           :T_size_t;
          minTexelBufferOffsetAlignment                   :T_VkDeviceSize;
          minUniformBufferOffsetAlignment                 :T_VkDeviceSize;
          minStorageBufferOffsetAlignment                 :T_VkDeviceSize;
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
          framebufferColorSampleCounts                    :T_VkSampleCountFlags;
          framebufferDepthSampleCounts                    :T_VkSampleCountFlags;
          framebufferStencilSampleCounts                  :T_VkSampleCountFlags;
          framebufferNoAttachmentsSampleCounts            :T_VkSampleCountFlags;
          maxColorAttachments                             :T_uint32_t;
          sampledImageColorSampleCounts                   :T_VkSampleCountFlags;
          sampledImageIntegerSampleCounts                 :T_VkSampleCountFlags;
          sampledImageDepthSampleCounts                   :T_VkSampleCountFlags;
          sampledImageStencilSampleCounts                 :T_VkSampleCountFlags;
          storageImageSampleCounts                        :T_VkSampleCountFlags;
          maxSampleMaskWords                              :T_uint32_t;
          timestampComputeAndGraphics                     :T_VkBool32;
          timestampPeriod                                 :T_float;
          maxClipDistances                                :T_uint32_t;
          maxCullDistances                                :T_uint32_t;
          maxCombinedClipAndCullDistances                 :T_uint32_t;
          discreteQueuePriorities                         :T_uint32_t;
          pointSizeRange                                  :array [ 0..1 ] of T_float;
          lineWidthRange                                  :array [ 0..1 ] of T_float;
          pointSizeGranularity                            :T_float;
          lineWidthGranularity                            :T_float;
          strictLines                                     :T_VkBool32;
          standardSampleLocations                         :T_VkBool32;
          optimalBufferCopyOffsetAlignment                :T_VkDeviceSize;
          optimalBufferCopyRowPitchAlignment              :T_VkDeviceSize;
          nonCoherentAtomSize                             :T_VkDeviceSize;
     end;

type P_VkPhysicalDeviceSparseProperties = ^T_VkPhysicalDeviceSparseProperties;
     T_VkPhysicalDeviceSparseProperties = record
          residencyStandard2DBlockShape            :T_VkBool32;
          residencyStandard2DMultisampleBlockShape :T_VkBool32;
          residencyStandard3DBlockShape            :T_VkBool32;
          residencyAlignedMipSize                  :T_VkBool32;
          residencyNonResidentStrict               :T_VkBool32;
     end;

type P_VkPhysicalDeviceProperties = ^T_VkPhysicalDeviceProperties;
     T_VkPhysicalDeviceProperties = record
          apiVersion        :T_uint32_t;
          driverVersion     :T_uint32_t;
          vendorID          :T_uint32_t;
          deviceID          :T_uint32_t;
          deviceType        :T_VkPhysicalDeviceType;
          deviceName        :array [ 0..VK_MAX_PHYSICAL_DEVICE_NAME_SIZE-1 ] of T_char;
          pipelineCacheUUID :array [ 0..VK_UUID_SIZE-1 ] of T_uint8_t;
          limits            :T_VkPhysicalDeviceLimits;
          sparseProperties  :T_VkPhysicalDeviceSparseProperties;
     end;

type P_VkQueueFamilyProperties = ^T_VkQueueFamilyProperties;
     T_VkQueueFamilyProperties = record
          queueFlags                  :T_VkQueueFlags;
          queueCount                  :T_uint32_t;
          timestampValidBits          :T_uint32_t;
          minImageTransferGranularity :T_VkExtent3D;
     end;

type P_VkMemoryType = ^T_VkMemoryType;
     T_VkMemoryType = record
          propertyFlags :T_VkMemoryPropertyFlags;
          heapIndex     :T_uint32_t;
     end;

type P_VkMemoryHeap = ^T_VkMemoryHeap;
     T_VkMemoryHeap = record
          size  :T_VkDeviceSize;
          flags :T_VkMemoryHeapFlags;
     end;

type P_VkPhysicalDeviceMemoryProperties = ^T_VkPhysicalDeviceMemoryProperties;
     T_VkPhysicalDeviceMemoryProperties = record
          memoryTypeCount :T_uint32_t;
          memoryTypes     :array [ 0..VK_MAX_MEMORY_TYPES-1 ] of T_VkMemoryType;
          memoryHeapCount :T_uint32_t;
          memoryHeaps     :array [ 0..VK_MAX_MEMORY_HEAPS-1 ] of T_VkMemoryHeap;
     end;

type P_VkDeviceQueueCreateInfo = ^T_VkDeviceQueueCreateInfo;
     T_VkDeviceQueueCreateInfo = record
         sType             :T_VkStructureType;
         pNext             :P_void;
          flags            :T_VkDeviceQueueCreateFlags;
          queueFamilyIndex :T_uint32_t;
          queueCount       :T_uint32_t;
         pQueuePriorities  :P_float;
     end;

type P_VkDeviceCreateInfo = ^T_VkDeviceCreateInfo;
     T_VkDeviceCreateInfo = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkDeviceCreateFlags;
          queueCreateInfoCount  :T_uint32_t;
         pQueueCreateInfos      :P_VkDeviceQueueCreateInfo;
          enabledLayerCount     :T_uint32_t;
        ppEnabledLayerNames     :PP_char;
          enabledExtensionCount :T_uint32_t;
        ppEnabledExtensionNames :PP_char;
         pEnabledFeatures       :P_VkPhysicalDeviceFeatures;
     end;

type P_VkExtensionProperties = ^T_VkExtensionProperties;
     T_VkExtensionProperties = record
          extensionName :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
          specVersion   :T_uint32_t;
     end;

type P_VkLayerProperties = ^T_VkLayerProperties;
     T_VkLayerProperties = record
          layerName             :array [ 0..VK_MAX_EXTENSION_NAME_SIZE-1 ] of T_char;
          specVersion           :T_uint32_t;
          implementationVersion :T_uint32_t;
          description           :array [ 0..VK_MAX_DESCRIPTION_SIZE-1 ] of T_char;
     end;

type P_VkSubmitInfo = ^T_VkSubmitInfo;
     T_VkSubmitInfo = record
         sType                 :T_VkStructureType;
         pNext                 :P_void;
          waitSemaphoreCount   :T_uint32_t;
         pWaitSemaphores       :P_VkSemaphore;
         pWaitDstStageMask     :P_VkPipelineStageFlags;
          commandBufferCount   :T_uint32_t;
         pCommandBuffers       :P_VkCommandBuffer;
          signalSemaphoreCount :T_uint32_t;
         pSignalSemaphores     :P_VkSemaphore;
     end;

type P_VkMemoryAllocateInfo = ^T_VkMemoryAllocateInfo;
     T_VkMemoryAllocateInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          allocationSize  :T_VkDeviceSize;
          memoryTypeIndex :T_uint32_t;
     end;

type P_VkMappedMemoryRange = ^T_VkMappedMemoryRange;
     T_VkMappedMemoryRange = record
         sType   :T_VkStructureType;
         pNext   :P_void;
          memory :T_VkDeviceMemory;
          offset :T_VkDeviceSize;
          size   :T_VkDeviceSize;
     end;

type P_VkMemoryRequirements = ^T_VkMemoryRequirements;
     T_VkMemoryRequirements = record
          size           :T_VkDeviceSize;
          alignment      :T_VkDeviceSize;
          memoryTypeBits :T_uint32_t;
     end;

type P_VkSparseImageFormatProperties = ^T_VkSparseImageFormatProperties;
     T_VkSparseImageFormatProperties = record
          aspectMask       :T_VkImageAspectFlags;
          imageGranularity :T_VkExtent3D;
          flags            :T_VkSparseImageFormatFlags;
     end;

type P_VkSparseImageMemoryRequirements = ^T_VkSparseImageMemoryRequirements;
     T_VkSparseImageMemoryRequirements = record
          formatProperties     :T_VkSparseImageFormatProperties;
          imageMipTailFirstLod :T_uint32_t;
          imageMipTailSize     :T_VkDeviceSize;
          imageMipTailOffset   :T_VkDeviceSize;
          imageMipTailStride   :T_VkDeviceSize;
     end;

type P_VkSparseMemoryBind = ^T_VkSparseMemoryBind;
     T_VkSparseMemoryBind = record
          resourceOffset :T_VkDeviceSize;
          size           :T_VkDeviceSize;
          memory         :T_VkDeviceMemory;
          memoryOffset   :T_VkDeviceSize;
          flags          :T_VkSparseMemoryBindFlags;
     end;

type P_VkSparseBufferMemoryBindInfo = ^T_VkSparseBufferMemoryBindInfo;
     T_VkSparseBufferMemoryBindInfo = record
          buffer    :T_VkBuffer;
          bindCount :T_uint32_t;
         pBinds     :P_VkSparseMemoryBind;
     end;

type P_VkSparseImageOpaqueMemoryBindInfo = ^T_VkSparseImageOpaqueMemoryBindInfo;
     T_VkSparseImageOpaqueMemoryBindInfo = record
          image     :T_VkImage;
          bindCount :T_uint32_t;
         pBinds     :P_VkSparseMemoryBind;
     end;

type P_VkImageSubresource = ^T_VkImageSubresource;
     T_VkImageSubresource = record
          aspectMask :T_VkImageAspectFlags;
       mipLevel      :T_uint32_t;
          arrayLayer :T_uint32_t;
     end;

type P_VkOffset3D = ^T_VkOffset3D;
     T_VkOffset3D = record
          x :T_int32_t;
          y :T_int32_t;
          z :T_int32_t;
     end;

type P_VkSparseImageMemoryBind = ^T_VkSparseImageMemoryBind;
     T_VkSparseImageMemoryBind = record
          subresource  :T_VkImageSubresource;
          offset       :T_VkOffset3D;
          extent       :T_VkExtent3D;
          memory       :T_VkDeviceMemory;
          memoryOffset :T_VkDeviceSize;
          flags        :T_VkSparseMemoryBindFlags;
     end;

type P_VkSparseImageMemoryBindInfo = ^T_VkSparseImageMemoryBindInfo;
     T_VkSparseImageMemoryBindInfo = record
          image     :T_VkImage;
          bindCount :T_uint32_t;
         pBinds     :P_VkSparseImageMemoryBind;
     end;

type P_VkBindSparseInfo = ^T_VkBindSparseInfo;
     T_VkBindSparseInfo = record
         sType                 :T_VkStructureType;
         pNext                 :P_void;
          waitSemaphoreCount   :T_uint32_t;
         pWaitSemaphores       :P_VkSemaphore;
          bufferBindCount      :T_uint32_t;
         pBufferBinds          :P_VkSparseBufferMemoryBindInfo;
          imageOpaqueBindCount :T_uint32_t;
         pImageOpaqueBinds     :P_VkSparseImageOpaqueMemoryBindInfo;
          imageBindCount       :T_uint32_t;
         pImageBinds           :P_VkSparseImageMemoryBindInfo;
          signalSemaphoreCount :T_uint32_t;
         pSignalSemaphores     :P_VkSemaphore;
     end;

type P_VkFenceCreateInfo = ^T_VkFenceCreateInfo;
     T_VkFenceCreateInfo = record
         sType  :T_VkStructureType;
         pNext  :P_void;
          flags :T_VkFenceCreateFlags;
     end;

type P_VkSemaphoreCreateInfo = ^T_VkSemaphoreCreateInfo;
     T_VkSemaphoreCreateInfo = record
         sType  :T_VkStructureType;
         pNext  :P_void;
          flags :T_VkSemaphoreCreateFlags;
     end;

type P_VkEventCreateInfo = ^T_VkEventCreateInfo;
     T_VkEventCreateInfo = record
         sType  :T_VkStructureType;
         pNext  :P_void;
          flags :T_VkEventCreateFlags;
     end;

type P_VkQueryPoolCreateInfo = ^T_VkQueryPoolCreateInfo;
     T_VkQueryPoolCreateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          flags              :T_VkQueryPoolCreateFlags;
          queryType          :T_VkQueryType;
          queryCount         :T_uint32_t;
          pipelineStatistics :T_VkQueryPipelineStatisticFlags;
     end;

type P_VkBufferCreateInfo = ^T_VkBufferCreateInfo;
     T_VkBufferCreateInfo = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkBufferCreateFlags;
          size                  :T_VkDeviceSize;
          usage                 :T_VkBufferUsageFlags;
          sharingMode           :T_VkSharingMode;
          queueFamilyIndexCount :T_uint32_t;
         pQueueFamilyIndices    :P_uint32_t;
     end;

type P_VkBufferViewCreateInfo = ^T_VkBufferViewCreateInfo;
     T_VkBufferViewCreateInfo = record
         sType   :T_VkStructureType;
         pNext   :P_void;
          flags  :T_VkBufferViewCreateFlags;
          buffer :T_VkBuffer;
          format :T_VkFormat;
          offset :T_VkDeviceSize;
          range  :T_VkDeviceSize;
     end;

type P_VkImageCreateInfo = ^T_VkImageCreateInfo;
     T_VkImageCreateInfo = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkImageCreateFlags;
          imageType             :T_VkImageType;
          format                :T_VkFormat;
          extent                :T_VkExtent3D;
          mipLevels             :T_uint32_t;
          arrayLayers           :T_uint32_t;
          samples               :T_VkSampleCountFlagBits;
          tiling                :T_VkImageTiling;
          usage                 :T_VkImageUsageFlags;
          sharingMode           :T_VkSharingMode;
          queueFamilyIndexCount :T_uint32_t;
         pQueueFamilyIndices    :P_uint32_t;
          initialLayout         :T_VkImageLayout;
     end;

type P_VkSubresourceLayout = ^T_VkSubresourceLayout;
     T_VkSubresourceLayout = record
          offset     :T_VkDeviceSize;
          size       :T_VkDeviceSize;
          rowPitch   :T_VkDeviceSize;
          arrayPitch :T_VkDeviceSize;
          depthPitch :T_VkDeviceSize;
     end;

type P_VkComponentMapping = ^T_VkComponentMapping;
     T_VkComponentMapping = record
          r :T_VkComponentSwizzle;
          g :T_VkComponentSwizzle;
          b :T_VkComponentSwizzle;
          a :T_VkComponentSwizzle;
     end;

type P_VkImageSubresourceRange = ^T_VkImageSubresourceRange;
     T_VkImageSubresourceRange = record
          aspectMask     :T_VkImageAspectFlags;
          baseMipLevel   :T_uint32_t;
          levelCount     :T_uint32_t;
          baseArrayLayer :T_uint32_t;
          layerCount     :T_uint32_t;
     end;

type P_VkImageViewCreateInfo = ^T_VkImageViewCreateInfo;
     T_VkImageViewCreateInfo = record
         sType             :T_VkStructureType;
         pNext             :P_void;
          flags            :T_VkImageViewCreateFlags;
          image            :T_VkImage;
          viewType         :T_VkImageViewType;
          format           :T_VkFormat;
          components       :T_VkComponentMapping;
          subresourceRange :T_VkImageSubresourceRange;
     end;

type P_VkShaderModuleCreateInfo = ^T_VkShaderModuleCreateInfo;
     T_VkShaderModuleCreateInfo = record
         sType     :T_VkStructureType;
         pNext     :P_void;
          flags    :T_VkShaderModuleCreateFlags;
          codeSize :T_size_t;
         pCode     :P_uint32_t;
     end;

type P_VkPipelineCacheCreateInfo = ^T_VkPipelineCacheCreateInfo;
     T_VkPipelineCacheCreateInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          flags           :T_VkPipelineCacheCreateFlags;
          initialDataSize :T_size_t;
         pInitialData     :P_void;
     end;

type P_VkSpecializationMapEntry = ^T_VkSpecializationMapEntry;
     T_VkSpecializationMapEntry = record
          constantID  :T_uint32_t;
          offset      :T_uint32_t;
          size        :T_size_t;
     end;

type P_VkSpecializationInfo = ^T_VkSpecializationInfo;
     T_VkSpecializationInfo = record
       mapEntryCount :T_uint32_t;
         pMapEntries :P_VkSpecializationMapEntry;
          dataSize   :T_size_t;
         pData       :P_void;
     end;

type P_VkPipelineShaderStageCreateInfo = ^T_VkPipelineShaderStageCreateInfo;
     T_VkPipelineShaderStageCreateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          flags              :T_VkPipelineShaderStageCreateFlags;
          stage              :T_VkShaderStageFlagBits;
          module             :T_VkShaderModule;
         pName               :P_char;
         pSpecializationInfo :P_VkSpecializationInfo;
     end;

type P_VkVertexInputBindingDescription = ^T_VkVertexInputBindingDescription;
     T_VkVertexInputBindingDescription = record
          binding   :T_uint32_t;
          stride    :T_uint32_t;
          inputRate :T_VkVertexInputRate;
     end;

type P_VkVertexInputAttributeDescription = ^T_VkVertexInputAttributeDescription;
     T_VkVertexInputAttributeDescription = record
          location :T_uint32_t;
          binding  :T_uint32_t;
          format   :T_VkFormat;
          offset   :T_uint32_t;
     end;

type P_VkPipelineVertexInputStateCreateInfo = ^T_VkPipelineVertexInputStateCreateInfo;
     T_VkPipelineVertexInputStateCreateInfo = record
         sType                            :T_VkStructureType;
         pNext                            :P_void;
          flags                           :T_VkPipelineVertexInputStateCreateFlags;
          vertexBindingDescriptionCount   :T_uint32_t;
         pVertexBindingDescriptions       :P_VkVertexInputBindingDescription;
          vertexAttributeDescriptionCount :T_uint32_t;
         pVertexAttributeDescriptions     :P_VkVertexInputAttributeDescription;
     end;

type P_VkPipelineInputAssemblyStateCreateInfo = ^T_VkPipelineInputAssemblyStateCreateInfo;
     T_VkPipelineInputAssemblyStateCreateInfo = record
         sType                   :T_VkStructureType;
         pNext                   :P_void;
          flags                  :T_VkPipelineInputAssemblyStateCreateFlags;
          topology               :T_VkPrimitiveTopology;
          primitiveRestartEnable :T_VkBool32;
     end;

type P_VkPipelineTessellationStateCreateInfo = ^T_VkPipelineTessellationStateCreateInfo;
     T_VkPipelineTessellationStateCreateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          flags              :T_VkPipelineTessellationStateCreateFlags;
          patchControlPoints :T_uint32_t;
     end;

type P_VkViewport = ^T_VkViewport;
     T_VkViewport = record
          x        :T_float;
          y        :T_float;
          width    :T_float;
          height   :T_float;
          minDepth :T_float;
          maxDepth :T_float;
     end;

type P_VkOffset2D = ^T_VkOffset2D;
     T_VkOffset2D = record
          x :T_int32_t;
          y :T_int32_t;
     end;

type P_VkExtent2D = ^T_VkExtent2D;
     T_VkExtent2D = record
          width  :T_uint32_t;
          height :T_uint32_t;
     end;

type P_VkRect2D = ^T_VkRect2D;
     T_VkRect2D = record
          offset :T_VkOffset2D;
          extent :T_VkExtent2D;
     end;

type P_VkPipelineViewportStateCreateInfo = ^T_VkPipelineViewportStateCreateInfo;
     T_VkPipelineViewportStateCreateInfo = record
         sType          :T_VkStructureType;
         pNext          :P_void;
          flags         :T_VkPipelineViewportStateCreateFlags;
          viewportCount :T_uint32_t;
         pViewports     :P_VkViewport;
          scissorCount  :T_uint32_t;
         pScissors      :P_VkRect2D;
     end;

type P_VkPipelineRasterizationStateCreateInfo = ^T_VkPipelineRasterizationStateCreateInfo;
     T_VkPipelineRasterizationStateCreateInfo = record
         sType                    :T_VkStructureType;
         pNext                    :P_void;
          flags                   :T_VkPipelineRasterizationStateCreateFlags;
          depthClampEnable        :T_VkBool32;
          rasterizerDiscardEnable :T_VkBool32;
          polygonMode             :T_VkPolygonMode;
          cullMode                :T_VkCullModeFlags;
          frontFace               :T_VkFrontFace;
          depthBiasEnable         :T_VkBool32;
          depthBiasConstantFactor :T_float;
          depthBiasClamp          :T_float;
          depthBiasSlopeFactor    :T_float;
          lineWidth               :T_float;
     end;

type P_VkPipelineMultisampleStateCreateInfo = ^T_VkPipelineMultisampleStateCreateInfo;
     T_VkPipelineMultisampleStateCreateInfo = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkPipelineMultisampleStateCreateFlags;
          rasterizationSamples  :T_VkSampleCountFlagBits;
          sampleShadingEnable   :T_VkBool32;
          minSampleShading      :T_float;
         pSampleMask            :P_VkSampleMask;
          alphaToCoverageEnable :T_VkBool32;
          alphaToOneEnable      :T_VkBool32;
     end;

type P_VkStencilOpState = ^T_VkStencilOpState;
     T_VkStencilOpState = record
          failOp      :T_VkStencilOp;
          passOp      :T_VkStencilOp;
          depthFailOp :T_VkStencilOp;
          compareOp   :T_VkCompareOp;
          compareMask :T_uint32_t;
          writeMask   :T_uint32_t;
          reference   :T_uint32_t;
     end;

type P_VkPipelineDepthStencilStateCreateInfo = ^T_VkPipelineDepthStencilStateCreateInfo;
     T_VkPipelineDepthStencilStateCreateInfo = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkPipelineDepthStencilStateCreateFlags;
          depthTestEnable       :T_VkBool32;
          depthWriteEnable      :T_VkBool32;
          depthCompareOp        :T_VkCompareOp;
          depthBoundsTestEnable :T_VkBool32;
          stencilTestEnable     :T_VkBool32;
          front                 :T_VkStencilOpState;
          back                  :T_VkStencilOpState;
          minDepthBounds        :T_float;
          maxDepthBounds        :T_float;
     end;

type P_VkPipelineColorBlendAttachmentState = ^T_VkPipelineColorBlendAttachmentState;
     T_VkPipelineColorBlendAttachmentState = record
          blendEnable         :T_VkBool32;
          srcColorBlendFactor :T_VkBlendFactor;
          dstColorBlendFactor :T_VkBlendFactor;
          colorBlendOp        :T_VkBlendOp;
          srcAlphaBlendFactor :T_VkBlendFactor;
          dstAlphaBlendFactor :T_VkBlendFactor;
          alphaBlendOp        :T_VkBlendOp;
          colorWriteMask      :T_VkColorComponentFlags;
     end;

type P_VkPipelineColorBlendStateCreateInfo = ^T_VkPipelineColorBlendStateCreateInfo;
     T_VkPipelineColorBlendStateCreateInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          flags           :T_VkPipelineColorBlendStateCreateFlags;
          logicOpEnable   :T_VkBool32;
          logicOp         :T_VkLogicOp;
          attachmentCount :T_uint32_t;
         pAttachments     :P_VkPipelineColorBlendAttachmentState;
          blendConstants  :array [ 0..3 ] of T_float;
     end;

type P_VkPipelineDynamicStateCreateInfo = ^T_VkPipelineDynamicStateCreateInfo;
     T_VkPipelineDynamicStateCreateInfo = record
         sType              :T_VkStructureType;
         pNext              :P_void;
          flags             :T_VkPipelineDynamicStateCreateFlags;
          dynamicStateCount :T_uint32_t;
         pDynamicStates     :P_VkDynamicState;
     end;

type P_VkGraphicsPipelineCreateInfo = ^T_VkGraphicsPipelineCreateInfo;
     T_VkGraphicsPipelineCreateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          flags              :T_VkPipelineCreateFlags;
          stageCount         :T_uint32_t;
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
          layout             :T_VkPipelineLayout;
          renderPass         :T_VkRenderPass;
          subpass            :T_uint32_t;
          basePipelineHandle :T_VkPipeline;
          basePipelineIndex  :T_int32_t;
     end;

type P_VkComputePipelineCreateInfo = ^T_VkComputePipelineCreateInfo;
     T_VkComputePipelineCreateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          flags              :T_VkPipelineCreateFlags;
          stage              :T_VkPipelineShaderStageCreateInfo;
          layout             :T_VkPipelineLayout;
          basePipelineHandle :T_VkPipeline;
          basePipelineIndex  :T_int32_t;
     end;

type P_VkPushConstantRange = ^T_VkPushConstantRange;
     T_VkPushConstantRange = record
          stageFlags :T_VkShaderStageFlags;
          offset     :T_uint32_t;
          size       :T_uint32_t;
     end;

type P_VkPipelineLayoutCreateInfo = ^T_VkPipelineLayoutCreateInfo;
     T_VkPipelineLayoutCreateInfo = record
         sType                   :T_VkStructureType;
         pNext                   :P_void;
          flags                  :T_VkPipelineLayoutCreateFlags;
          setLayoutCount         :T_uint32_t;
         pSetLayouts             :P_VkDescriptorSetLayout;
          pushConstantRangeCount :T_uint32_t;
         pPushConstantRanges     :P_VkPushConstantRange;
     end;

type P_VkSamplerCreateInfo = ^T_VkSamplerCreateInfo;
     T_VkSamplerCreateInfo = record
         sType                    :T_VkStructureType;
         pNext                    :P_void;
          flags                   :T_VkSamplerCreateFlags;
          magFilter               :T_VkFilter;
          minFilter               :T_VkFilter;
          mipmapMode              :T_VkSamplerMipmapMode;
          addressModeU            :T_VkSamplerAddressMode;
          addressModeV            :T_VkSamplerAddressMode;
          addressModeW            :T_VkSamplerAddressMode;
          mipLodBias              :T_float;
          anisotropyEnable        :T_VkBool32;
          maxAnisotropy           :T_float;
          compareEnable           :T_VkBool32;
          compareOp               :T_VkCompareOp;
          minLod                  :T_float;
          maxLod                  :T_float;
          borderColor             :T_VkBorderColor;
          unnormalizedCoordinates :T_VkBool32;
     end;

type P_VkDescriptorSetLayoutBinding = ^T_VkDescriptorSetLayoutBinding;
     T_VkDescriptorSetLayoutBinding = record
          binding            :T_uint32_t;
          descriptorType     :T_VkDescriptorType;
          descriptorCount    :T_uint32_t;
          stageFlags         :T_VkShaderStageFlags;
         pImmutableSamplers  :P_VkSampler;
     end;

type P_VkDescriptorSetLayoutCreateInfo = ^T_VkDescriptorSetLayoutCreateInfo;
     T_VkDescriptorSetLayoutCreateInfo = record
         sType         :T_VkStructureType;
         pNext         :P_void;
          flags        :T_VkDescriptorSetLayoutCreateFlags;
          bindingCount :T_uint32_t;
         pBindings     :P_VkDescriptorSetLayoutBinding;
     end;

type P_VkDescriptorPoolSize = ^T_VkDescriptorPoolSize;
     T_VkDescriptorPoolSize = record
          type_           :T_VkDescriptorType;
          descriptorCount :T_uint32_t;
     end;

type P_VkDescriptorPoolCreateInfo = ^T_VkDescriptorPoolCreateInfo;
     T_VkDescriptorPoolCreateInfo = record
         sType          :T_VkStructureType;
         pNext          :P_void;
          flags         :T_VkDescriptorPoolCreateFlags;
          maxSets       :T_uint32_t;
          poolSizeCount :T_uint32_t;
         pPoolSizes     :P_VkDescriptorPoolSize;
     end;

type P_VkDescriptorSetAllocateInfo = ^T_VkDescriptorSetAllocateInfo;
     T_VkDescriptorSetAllocateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          descriptorPool     :T_VkDescriptorPool;
          descriptorSetCount :T_uint32_t;
         pSetLayouts         :P_VkDescriptorSetLayout;
     end;

type P_VkDescriptorImageInfo = ^T_VkDescriptorImageInfo;
     T_VkDescriptorImageInfo = record
          sampler     :T_VkSampler;
          imageView   :T_VkImageView;
          imageLayout :T_VkImageLayout;
     end;

type P_VkDescriptorBufferInfo = ^T_VkDescriptorBufferInfo;
     T_VkDescriptorBufferInfo = record
          buffer :T_VkBuffer;
          offset :T_VkDeviceSize;
          range  :T_VkDeviceSize;
     end;

type P_VkWriteDescriptorSet = ^T_VkWriteDescriptorSet;
     T_VkWriteDescriptorSet = record
         sType            :T_VkStructureType;
         pNext            :P_void;
         dstSet           :T_VkDescriptorSet;
          dstBinding      :T_uint32_t;
          dstArrayElement :T_uint32_t;
          descriptorCount :T_uint32_t;
          descriptorType  :T_VkDescriptorType;
         pImageInfo       :P_VkDescriptorImageInfo;
         pBufferInfo      :P_VkDescriptorBufferInfo;
         pTexelBufferView :P_VkBufferView;
     end;

type P_VkCopyDescriptorSet = ^T_VkCopyDescriptorSet;
     T_VkCopyDescriptorSet = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          srcSet          :T_VkDescriptorSet;
          srcBinding      :T_uint32_t;
          srcArrayElement :T_uint32_t;
          dstSet          :T_VkDescriptorSet;
          dstBinding      :T_uint32_t;
          dstArrayElement :T_uint32_t;
          descriptorCount :T_uint32_t;
     end;

type P_VkFramebufferCreateInfo = ^T_VkFramebufferCreateInfo;
     T_VkFramebufferCreateInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          flags           :T_VkFramebufferCreateFlags;
          renderPass      :T_VkRenderPass;
          attachmentCount :T_uint32_t;
         pAttachments     :P_VkImageView;
          width           :T_uint32_t;
          height          :T_uint32_t;
          layers          :T_uint32_t;
     end;

type P_VkAttachmentDescription = ^T_VkAttachmentDescription;
     T_VkAttachmentDescription = record
          flags          :T_VkAttachmentDescriptionFlags;
          format         :T_VkFormat;
          samples        :T_VkSampleCountFlagBits;
          loadOp         :T_VkAttachmentLoadOp;
          storeOp        :T_VkAttachmentStoreOp;
          stencilLoadOp  :T_VkAttachmentLoadOp;
          stencilStoreOp :T_VkAttachmentStoreOp;
          initialLayout  :T_VkImageLayout;
          finalLayout    :T_VkImageLayout;
     end;

type P_VkAttachmentReference = ^T_VkAttachmentReference;
     T_VkAttachmentReference = record
          attachment :T_uint32_t;
          layout     :T_VkImageLayout;
     end;

type P_VkSubpassDescription = ^T_VkSubpassDescription;
     T_VkSubpassDescription = record
          flags                   :T_VkSubpassDescriptionFlags;
          pipelineBindPoint       :T_VkPipelineBindPoint;
          inputAttachmentCount    :T_uint32_t;
         pInputAttachments        :P_VkAttachmentReference;
          colorAttachmentCount    :T_uint32_t;
         pColorAttachments        :P_VkAttachmentReference;
         pResolveAttachments      :P_VkAttachmentReference;
         pDepthStencilAttachment  :P_VkAttachmentReference;
          preserveAttachmentCount :T_uint32_t;
         pPreserveAttachments     :P_uint32_t;
     end;

type P_VkSubpassDependency = ^T_VkSubpassDependency;
     T_VkSubpassDependency = record
          srcSubpass      :T_uint32_t;
          dstSubpass      :T_uint32_t;
          srcStageMask    :T_VkPipelineStageFlags;
          dstStageMask    :T_VkPipelineStageFlags;
          srcAccessMask   :T_VkAccessFlags;
          dstAccessMask   :T_VkAccessFlags;
          dependencyFlags :T_VkDependencyFlags;
     end;

type P_VkRenderPassCreateInfo = ^T_VkRenderPassCreateInfo;
     T_VkRenderPassCreateInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          flags           :T_VkRenderPassCreateFlags;
          attachmentCount :T_uint32_t;
         pAttachments     :P_VkAttachmentDescription;
          subpassCount    :T_uint32_t;
         pSubpasses       :P_VkSubpassDescription;
          dependencyCount :T_uint32_t;
         pDependencies    :P_VkSubpassDependency;
     end;

type P_VkCommandPoolCreateInfo = ^T_VkCommandPoolCreateInfo;
     T_VkCommandPoolCreateInfo = record
         sType             :T_VkStructureType;
         pNext             :P_void;
          flags            :T_VkCommandPoolCreateFlags;
          queueFamilyIndex :T_uint32_t;
     end;

type P_VkCommandBufferAllocateInfo = ^T_VkCommandBufferAllocateInfo;
     T_VkCommandBufferAllocateInfo = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          commandPool        :T_VkCommandPool;
          level              :T_VkCommandBufferLevel;
          commandBufferCount :T_uint32_t;
     end;

type P_VkCommandBufferInheritanceInfo = ^T_VkCommandBufferInheritanceInfo;
     T_VkCommandBufferInheritanceInfo = record
         sType                 :T_VkStructureType;
         pNext                 :P_void;
          renderPass           :T_VkRenderPass;
          subpass              :T_uint32_t;
          framebuffer          :T_VkFramebuffer;
          occlusionQueryEnable :T_VkBool32;
          queryFlags           :T_VkQueryControlFlags;
          pipelineStatistics   :T_VkQueryPipelineStatisticFlags;
     end;

type P_VkCommandBufferBeginInfo = ^T_VkCommandBufferBeginInfo;
     T_VkCommandBufferBeginInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          flags           :T_VkCommandBufferUsageFlags;
         pInheritanceInfo :P_VkCommandBufferInheritanceInfo;
     end;

type P_VkBufferCopy = ^T_VkBufferCopy;
     T_VkBufferCopy = record
          srcOffset :T_VkDeviceSize;
          dstOffset :T_VkDeviceSize;
          size      :T_VkDeviceSize;
     end;

type P_VkImageSubresourceLayers = ^T_VkImageSubresourceLayers;
     T_VkImageSubresourceLayers = record
          aspectMask     :T_VkImageAspectFlags;
          mipLevel       :T_uint32_t;
          baseArrayLayer :T_uint32_t;
          layerCount     :T_uint32_t;
     end;

type P_VkImageCopy = ^T_VkImageCopy;
     T_VkImageCopy = record
          srcSubresource :T_VkImageSubresourceLayers;
          srcOffset      :T_VkOffset3D;
          dstSubresource :T_VkImageSubresourceLayers;
          dstOffset      :T_VkOffset3D;
          extent         :T_VkExtent3D;
     end;

type P_VkImageBlit = ^T_VkImageBlit;
     T_VkImageBlit = record
          srcSubresource :T_VkImageSubresourceLayers;
          srcOffsets     :array [ 0..1 ] of T_VkOffset3D;
          dstSubresource :T_VkImageSubresourceLayers;
          dstOffsets     :array [ 0..1 ] of T_VkOffset3D;
     end;

type P_VkBufferImageCopy = ^T_VkBufferImageCopy;
     T_VkBufferImageCopy = record
          bufferOffset      :T_VkDeviceSize;
          bufferRowLength   :T_uint32_t;
          bufferImageHeight :T_uint32_t;
          imageSubresource  :T_VkImageSubresourceLayers;
          imageOffset       :T_VkOffset3D;
          imageExtent       :T_VkExtent3D;
     end;

type P_VkClearColorValue = ^T_VkClearColorValue;
     T_VkClearColorValue = record
     case Integer of
       0:( T_float32 :array [ 0..3 ] of T_float;    );
       1:( int32     :array [ 0..3 ] of T_int32_t;  );
       2:( uint32    :array [ 0..3 ] of T_uint32_t; );
     end;

type P_VkClearDepthStencilValue = ^T_VkClearDepthStencilValue;
     T_VkClearDepthStencilValue = record
          depth   :T_float;
          stencil :T_uint32_t;
     end;

type P_VkClearValue = ^T_VkClearValue;
     T_VkClearValue = record
     case Integer of
       0:( color        :T_VkClearColorValue;        );
       1:( depthStencil :T_VkClearDepthStencilValue; );
     end;

type P_VkClearAttachment = ^T_VkClearAttachment;
     T_VkClearAttachment = record
          aspectMask      :T_VkImageAspectFlags;
          colorAttachment :T_uint32_t;
          clearValue      :T_VkClearValue;
     end;

type P_VkClearRect = ^T_VkClearRect;
     T_VkClearRect = record
          rect           :T_VkRect2D;
          baseArrayLayer :T_uint32_t;
          layerCount     :T_uint32_t;
     end;

type P_VkImageResolve = ^T_VkImageResolve;
     T_VkImageResolve = record
          srcSubresource :T_VkImageSubresourceLayers;
          srcOffset      :T_VkOffset3D;
          dstSubresource :T_VkImageSubresourceLayers;
          dstOffset      :T_VkOffset3D;
          extent         :T_VkExtent3D;
     end;

type P_VkMemoryBarrier = ^T_VkMemoryBarrier;
     T_VkMemoryBarrier = record
         sType          :T_VkStructureType;
         pNext          :P_void;
          srcAccessMask :T_VkAccessFlags;
          dstAccessMask :T_VkAccessFlags;
     end;

type P_VkBufferMemoryBarrier = ^T_VkBufferMemoryBarrier;
     T_VkBufferMemoryBarrier = record
         sType                :T_VkStructureType;
         pNext                :P_void;
          srcAccessMask       :T_VkAccessFlags;
          dstAccessMask       :T_VkAccessFlags;
          srcQueueFamilyIndex :T_uint32_t;
          dstQueueFamilyIndex :T_uint32_t;
          buffer              :T_VkBuffer;
          offset              :T_VkDeviceSize;
          size                :T_VkDeviceSize;
     end;

type P_VkImageMemoryBarrier = ^T_VkImageMemoryBarrier;
     T_VkImageMemoryBarrier = record
         sType                :T_VkStructureType;
         pNext                :P_void;
          srcAccessMask       :T_VkAccessFlags;
          dstAccessMask       :T_VkAccessFlags;
          oldLayout           :T_VkImageLayout;
          newLayout           :T_VkImageLayout;
          srcQueueFamilyIndex :T_uint32_t;
          dstQueueFamilyIndex :T_uint32_t;
          image               :T_VkImage;
          subresourceRange    :T_VkImageSubresourceRange;
     end;

type P_VkRenderPassBeginInfo = ^T_VkRenderPassBeginInfo;
     T_VkRenderPassBeginInfo = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          renderPass      :T_VkRenderPass;
          framebuffer     :T_VkFramebuffer;
          renderArea      :T_VkRect2D;
          clearValueCount :T_uint32_t;
         pClearValues     :P_VkClearValue;
     end;

type P_VkDispatchIndirectCommand = ^T_VkDispatchIndirectCommand;
     T_VkDispatchIndirectCommand = record
          x :T_uint32_t;
          y :T_uint32_t;
          z :T_uint32_t;
     end;

type P_VkDrawIndexedIndirectCommand = ^T_VkDrawIndexedIndirectCommand;
     T_VkDrawIndexedIndirectCommand = record
          indexCount    :T_uint32_t;
          instanceCount :T_uint32_t;
          firstIndex    :T_uint32_t;
          vertexOffset  :T_int32_t;
          firstInstance :T_uint32_t;
     end;

type P_VkDrawIndirectCommand = ^T_VkDrawIndirectCommand;
     T_VkDrawIndirectCommand = record
          vertexCount   :T_uint32_t;
          instanceCount :T_uint32_t;
          firstVertex   :T_uint32_t;
          firstInstance :T_uint32_t;
     end;

type PFN_vkCreateInstance                               = function( const pCreateInfo_:P_VkInstanceCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pInstance_:P_VkInstance ) :T_VkResult; stdcall;
type PFN_vkDestroyInstance                              = procedure( instance_:T_VkInstance; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkEnumeratePhysicalDevices                     = function( instance_:T_VkInstance; pPhysicalDeviceCount_:P_uint32_t; pPhysicalDevices_:P_VkPhysicalDevice ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceFeatures                    = procedure( physicalDevice_:T_VkPhysicalDevice; pFeatures_:P_VkPhysicalDeviceFeatures ); stdcall;
type PFN_vkGetPhysicalDeviceFormatProperties            = procedure( physicalDevice_:T_VkPhysicalDevice; format_:T_VkFormat; pFormatProperties_:P_VkFormatProperties ); stdcall;
type PFN_vkGetPhysicalDeviceImageFormatProperties       = function( physicalDevice_:T_VkPhysicalDevice; format_:T_VkFormat; type_:T_VkImageType; tiling_:T_VkImageTiling; usage_:T_VkImageUsageFlags; flags_:T_VkImageCreateFlags; pImageFormatProperties_:P_VkImageFormatProperties ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceProperties                  = procedure( physicalDevice_:T_VkPhysicalDevice; pProperties_:P_VkPhysicalDeviceProperties ); stdcall;
type PFN_vkGetPhysicalDeviceQueueFamilyProperties       = procedure( physicalDevice_:T_VkPhysicalDevice; pQueueFamilyPropertyCount_:P_uint32_t; pQueueFamilyProperties_:P_VkQueueFamilyProperties ); stdcall;
type PFN_vkGetPhysicalDeviceMemoryProperties            = procedure( physicalDevice_:T_VkPhysicalDevice; pMemoryProperties_:P_VkPhysicalDeviceMemoryProperties ); stdcall;
type PFN_vkGetInstanceProcAddr                          = function( instance_:T_VkInstance; const pName_:P_char ) :PFN_vkVoidFunction; stdcall;
type PFN_vkGetDeviceProcAddr                            = function( device_:T_VkDevice; const pName_:P_char ) :PFN_vkVoidFunction; stdcall;
type PFN_vkCreateDevice                                 = function( physicalDevice_:T_VkPhysicalDevice; const pCreateInfo_:P_VkDeviceCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pDevice_:P_VkDevice ) :T_VkResult; stdcall;
type PFN_vkDestroyDevice                                = procedure( device_:T_VkDevice; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkEnumerateInstanceExtensionProperties         = function( const pLayerName_:P_char; pPropertyCount_:P_uint32_t; pProperties_:P_VkExtensionProperties ) :T_VkResult; stdcall;
type PFN_vkEnumerateDeviceExtensionProperties           = function( physicalDevice_:T_VkPhysicalDevice; const pLayerName_:P_char; pPropertyCount_:P_uint32_t; pProperties_:P_VkExtensionProperties ) :T_VkResult; stdcall;
type PFN_vkEnumerateInstanceLayerProperties             = function( pPropertyCount_:P_uint32_t; pProperties_:P_VkLayerProperties ) :T_VkResult; stdcall;
type PFN_vkEnumerateDeviceLayerProperties               = function( physicalDevice_:T_VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkLayerProperties ) :T_VkResult; stdcall;
type PFN_vkGetDeviceQueue                               = procedure( device_:T_VkDevice; queueFamilyIndex_:T_uint32_t; queueIndex_:T_uint32_t; pQueue_:P_VkQueue ); stdcall;
type PFN_vkQueueSubmit                                  = function( queue_:T_VkQueue; submitCount_:T_uint32_t; const pSubmits_:P_VkSubmitInfo; fence_:T_VkFence ) :T_VkResult; stdcall;
type PFN_vkQueueWaitIdle                                = function( queue_:T_VkQueue ) :T_VkResult; stdcall;
type PFN_vkDeviceWaitIdle                               = function( device_:T_VkDevice ) :T_VkResult; stdcall;
type PFN_vkAllocateMemory                               = function( device_:T_VkDevice; const pAllocateInfo_:P_VkMemoryAllocateInfo; const pAllocator_:P_VkAllocationCallbacks; pMemory_:P_VkDeviceMemory ) :T_VkResult; stdcall;
type PFN_vkFreeMemory                                   = procedure( device_:T_VkDevice; memory_:T_VkDeviceMemory; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkMapMemory                                    = function( device_:T_VkDevice; memory_:T_VkDeviceMemory; offset_:T_VkDeviceSize; size_:T_VkDeviceSize; flags_:T_VkMemoryMapFlags; ppData_:PP_void ) :T_VkResult; stdcall;
type PFN_vkUnmapMemory                                  = procedure( device_:T_VkDevice; memory_:T_VkDeviceMemory ); stdcall;
type PFN_vkFlushMappedMemoryRanges                      = function( device_:T_VkDevice; memoryRangeCount_:T_uint32_t; const pMemoryRanges_:P_VkMappedMemoryRange ) :T_VkResult; stdcall;
type PFN_vkInvalidateMappedMemoryRanges                 = function( device_:T_VkDevice; memoryRangeCount_:T_uint32_t; const pMemoryRanges_:P_VkMappedMemoryRange ) :T_VkResult; stdcall;
type PFN_vkGetDeviceMemoryCommitment                    = procedure( device_:T_VkDevice; memory_:T_VkDeviceMemory; pCommittedMemoryInBytes_:P_VkDeviceSize ); stdcall;
type PFN_vkBindBufferMemory                             = function( device_:T_VkDevice; buffer_:T_VkBuffer; memory_:T_VkDeviceMemory; memoryOffset_:T_VkDeviceSize ) :T_VkResult; stdcall;
type PFN_vkBindImageMemory                              = function( device_:T_VkDevice; image_:T_VkImage; memory_:T_VkDeviceMemory; memoryOffset_:T_VkDeviceSize ) :T_VkResult; stdcall;
type PFN_vkGetBufferMemoryRequirements                  = procedure( device_:T_VkDevice; buffer_:T_VkBuffer; pMemoryRequirements_:P_VkMemoryRequirements ); stdcall;
type PFN_vkGetImageMemoryRequirements                   = procedure( device_:T_VkDevice; image_:T_VkImage; pMemoryRequirements_:P_VkMemoryRequirements ); stdcall;
type PFN_vkGetImageSparseMemoryRequirements             = procedure( device_:T_VkDevice; image_:T_VkImage; pSparseMemoryRequirementCount_:P_uint32_t; pSparseMemoryRequirements_:P_VkSparseImageMemoryRequirements ); stdcall;
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties = procedure( physicalDevice_:T_VkPhysicalDevice; format_:T_VkFormat; type_:T_VkImageType; samples_:T_VkSampleCountFlagBits; usage_:T_VkImageUsageFlags; tiling_:T_VkImageTiling; pPropertyCount_:P_uint32_t; pProperties_:P_VkSparseImageFormatProperties ); stdcall;
type PFN_vkQueueBindSparse                              = function( queue_:T_VkQueue; bindInfoCount_:T_uint32_t; const pBindInfo_:P_VkBindSparseInfo; fence_:T_VkFence ) :T_VkResult; stdcall;
type PFN_vkCreateFence                                  = function( device_:T_VkDevice; const pCreateInfo_:P_VkFenceCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pFence_:P_VkFence ) :T_VkResult; stdcall;
type PFN_vkDestroyFence                                 = procedure( device_:T_VkDevice; fence_:T_VkFence; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkResetFences                                  = function( device_:T_VkDevice; fenceCount_:T_uint32_t; const pFences_:P_VkFence ) :T_VkResult; stdcall;
type PFN_vkGetFenceStatus                               = function( device_:T_VkDevice; fence_:T_VkFence ) :T_VkResult; stdcall;
type PFN_vkWaitForFences                                = function( device_:T_VkDevice; fenceCount_:T_uint32_t; const pFences_:P_VkFence; waitAll_:T_VkBool32; timeout_:T_uint64_t ) :T_VkResult; stdcall;
type PFN_vkCreateSemaphore                              = function( device_:T_VkDevice; const pCreateInfo_:P_VkSemaphoreCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pSemaphore_:P_VkSemaphore ) :T_VkResult; stdcall;
type PFN_vkDestroySemaphore                             = procedure( device_:T_VkDevice; semaphore_:T_VkSemaphore; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateEvent                                  = function( device_:T_VkDevice; const pCreateInfo_:P_VkEventCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pEvent_:P_VkEvent ) :T_VkResult; stdcall;
type PFN_vkDestroyEvent                                 = procedure( device_:T_VkDevice; event_:T_VkEvent; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetEventStatus                               = function( device_:T_VkDevice; event_:T_VkEvent ) :T_VkResult; stdcall;
type PFN_vkSetEvent                                     = function( device_:T_VkDevice; event_:T_VkEvent ) :T_VkResult; stdcall;
type PFN_vkResetEvent                                   = function( device_:T_VkDevice; event_:T_VkEvent ) :T_VkResult; stdcall;
type PFN_vkCreateQueryPool                              = function( device_:T_VkDevice; const pCreateInfo_:P_VkQueryPoolCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pQueryPool_:P_VkQueryPool ) :T_VkResult; stdcall;
type PFN_vkDestroyQueryPool                             = procedure( device_:T_VkDevice; queryPool_:T_VkQueryPool; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetQueryPoolResults                          = function( device_:T_VkDevice; queryPool_:T_VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t; dataSize_:T_size_t; pData_:P_void; stride_:T_VkDeviceSize; flags_:T_VkQueryResultFlags ) :T_VkResult; stdcall;
type PFN_vkCreateBuffer                                 = function( device_:T_VkDevice; const pCreateInfo_:P_VkBufferCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pBuffer_:P_VkBuffer ) :T_VkResult; stdcall;
type PFN_vkDestroyBuffer                                = procedure( device_:T_VkDevice; buffer_:T_VkBuffer; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateBufferView                             = function( device_:T_VkDevice; const pCreateInfo_:P_VkBufferViewCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pView_:P_VkBufferView ) :T_VkResult; stdcall;
type PFN_vkDestroyBufferView                            = procedure( device_:T_VkDevice; bufferView_:T_VkBufferView; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateImage                                  = function( device_:T_VkDevice; const pCreateInfo_:P_VkImageCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pImage_:P_VkImage ) :T_VkResult; stdcall;
type PFN_vkDestroyImage                                 = procedure( device_:T_VkDevice; image_:T_VkImage; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetImageSubresourceLayout                    = procedure( device_:T_VkDevice; image_:T_VkImage; const pSubresource_:P_VkImageSubresource; pLayout_:P_VkSubresourceLayout ); stdcall;
type PFN_vkCreateImageView                              = function( device_:T_VkDevice; const pCreateInfo_:P_VkImageViewCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pView_:P_VkImageView ) :T_VkResult; stdcall;
type PFN_vkDestroyImageView                             = procedure( device_:T_VkDevice; imageView_:T_VkImageView; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateShaderModule                           = function( device_:T_VkDevice; const pCreateInfo_:P_VkShaderModuleCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pShaderModule_:P_VkShaderModule ) :T_VkResult; stdcall;
type PFN_vkDestroyShaderModule                          = procedure( device_:T_VkDevice; shaderModule_:T_VkShaderModule; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreatePipelineCache                          = function( device_:T_VkDevice; const pCreateInfo_:P_VkPipelineCacheCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelineCache_:P_VkPipelineCache ) :T_VkResult; stdcall;
type PFN_vkDestroyPipelineCache                         = procedure( device_:T_VkDevice; pipelineCache_:T_VkPipelineCache; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetPipelineCacheData                         = function( device_:T_VkDevice; pipelineCache_:T_VkPipelineCache; pDataSize_:P_size_t; pData_:P_void ) :T_VkResult; stdcall;
type PFN_vkMergePipelineCaches                          = function( device_:T_VkDevice; dstCache_:T_VkPipelineCache; srcCacheCount_:T_uint32_t; const pSrcCaches_:P_VkPipelineCache ) :T_VkResult; stdcall;
type PFN_vkCreateGraphicsPipelines                      = function( device_:T_VkDevice; pipelineCache_:T_VkPipelineCache; createInfoCount_:T_uint32_t; const pCreateInfos_:P_VkGraphicsPipelineCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelines_:P_VkPipeline ) :T_VkResult; stdcall;
type PFN_vkCreateComputePipelines                       = function( device_:T_VkDevice; pipelineCache_:T_VkPipelineCache; createInfoCount_:T_uint32_t; const pCreateInfos_:P_VkComputePipelineCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelines_:P_VkPipeline ) :T_VkResult; stdcall;
type PFN_vkDestroyPipeline                              = procedure( device_:T_VkDevice; pipeline_:T_VkPipeline; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreatePipelineLayout                         = function( device_:T_VkDevice; const pCreateInfo_:P_VkPipelineLayoutCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pPipelineLayout_:P_VkPipelineLayout ) :T_VkResult; stdcall;
type PFN_vkDestroyPipelineLayout                        = procedure( device_:T_VkDevice; pipelineLayout_:T_VkPipelineLayout; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateSampler                                = function( device_:T_VkDevice; const pCreateInfo_:P_VkSamplerCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pSampler_:P_VkSampler ) :T_VkResult; stdcall;
type PFN_vkDestroySampler                               = procedure( device_:T_VkDevice; sampler_:T_VkSampler; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateDescriptorSetLayout                    = function( device_:T_VkDevice; const pCreateInfo_:P_VkDescriptorSetLayoutCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pSetLayout_:P_VkDescriptorSetLayout ) :T_VkResult; stdcall;
type PFN_vkDestroyDescriptorSetLayout                   = procedure( device_:T_VkDevice; descriptorSetLayout_:T_VkDescriptorSetLayout; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateDescriptorPool                         = function( device_:T_VkDevice; const pCreateInfo_:P_VkDescriptorPoolCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pDescriptorPool_:P_VkDescriptorPool ) :T_VkResult; stdcall;
type PFN_vkDestroyDescriptorPool                        = procedure( device_:T_VkDevice; descriptorPool_:T_VkDescriptorPool; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkResetDescriptorPool                          = function( device_:T_VkDevice; descriptorPool_:T_VkDescriptorPool; flags_:T_VkDescriptorPoolResetFlags ) :T_VkResult; stdcall;
type PFN_vkAllocateDescriptorSets                       = function( device_:T_VkDevice; const pAllocateInfo_:P_VkDescriptorSetAllocateInfo; pDescriptorSets_:P_VkDescriptorSet ) :T_VkResult; stdcall;
type PFN_vkFreeDescriptorSets                           = function( device_:T_VkDevice; descriptorPool_:T_VkDescriptorPool; descriptorSetCount_:T_uint32_t; const pDescriptorSets_:P_VkDescriptorSet ) :T_VkResult; stdcall;
type PFN_vkUpdateDescriptorSets                         = procedure( device_:T_VkDevice; descriptorWriteCount_:T_uint32_t; const pDescriptorWrites_:P_VkWriteDescriptorSet; descriptorCopyCount_:T_uint32_t; const pDescriptorCopies_:P_VkCopyDescriptorSet ); stdcall;
type PFN_vkCreateFramebuffer                            = function( device_:T_VkDevice; const pCreateInfo_:P_VkFramebufferCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pFramebuffer_:P_VkFramebuffer ) :T_VkResult; stdcall;
type PFN_vkDestroyFramebuffer                           = procedure( device_:T_VkDevice; framebuffer_:T_VkFramebuffer; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkCreateRenderPass                             = function( device_:T_VkDevice; const pCreateInfo_:P_VkRenderPassCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pRenderPass_:P_VkRenderPass ) :T_VkResult; stdcall;
type PFN_vkDestroyRenderPass                            = procedure( device_:T_VkDevice; renderPass_:T_VkRenderPass; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetRenderAreaGranularity                     = procedure( device_:T_VkDevice; renderPass_:T_VkRenderPass; pGranularity_:P_VkExtent2D ); stdcall;
type PFN_vkCreateCommandPool                            = function( device_:T_VkDevice; const pCreateInfo_:P_VkCommandPoolCreateInfo; const pAllocator_:P_VkAllocationCallbacks; pCommandPool_:P_VkCommandPool ) :T_VkResult; stdcall;
type PFN_vkDestroyCommandPool                           = procedure( device_:T_VkDevice; commandPool_:T_VkCommandPool; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkResetCommandPool                             = function( device_:T_VkDevice; commandPool_:T_VkCommandPool; flags_:T_VkCommandPoolResetFlags ) :T_VkResult; stdcall;
type PFN_vkAllocateCommandBuffers                       = function( device_:T_VkDevice; const pAllocateInfo_:P_VkCommandBufferAllocateInfo; pCommandBuffers_:P_VkCommandBuffer ) :T_VkResult; stdcall;
type PFN_vkFreeCommandBuffers                           = procedure( device_:T_VkDevice; commandPool_:T_VkCommandPool; commandBufferCount_:T_uint32_t; const pCommandBuffers_:P_VkCommandBuffer ); stdcall;
type PFN_vkBeginCommandBuffer                           = function( commandBuffer_:T_VkCommandBuffer; const pBeginInfo_:P_VkCommandBufferBeginInfo ) :T_VkResult; stdcall;
type PFN_vkEndCommandBuffer                             = function( commandBuffer_:T_VkCommandBuffer ) :T_VkResult; stdcall;
type PFN_vkResetCommandBuffer                           = function( commandBuffer_:T_VkCommandBuffer; flags_:T_VkCommandBufferResetFlags ) :T_VkResult; stdcall;
type PFN_vkCmdBindPipeline                              = procedure( commandBuffer_:T_VkCommandBuffer; pipelineBindPoint_:T_VkPipelineBindPoint; pipeline_:T_VkPipeline ); stdcall;
type PFN_vkCmdSetViewport                               = procedure( commandBuffer_:T_VkCommandBuffer; firstViewport_:T_uint32_t; viewportCount_:T_uint32_t; const pViewports_:P_VkViewport ); stdcall;
type PFN_vkCmdSetScissor                                = procedure( commandBuffer_:T_VkCommandBuffer; firstScissor_:T_uint32_t; scissorCount_:T_uint32_t; const pScissors_:P_VkRect2D ); stdcall;
type PFN_vkCmdSetLineWidth                              = procedure( commandBuffer_:T_VkCommandBuffer; lineWidth_:T_float ); stdcall;
type PFN_vkCmdSetDepthBias                              = procedure( commandBuffer_:T_VkCommandBuffer; depthBiasConstantFactor_:T_float; depthBiasClamp_:T_float; depthBiasSlopeFactor_:T_float ); stdcall;
type PFN_vkCmdSetBlendConstants                         = procedure( commandBuffer_:T_VkCommandBuffer; const blendConstants_:T_float4 ); stdcall;
type PFN_vkCmdSetDepthBounds                            = procedure( commandBuffer_:T_VkCommandBuffer; minDepthBounds_:T_float; maxDepthBounds_:T_float ); stdcall;
type PFN_vkCmdSetStencilCompareMask                     = procedure( commandBuffer_:T_VkCommandBuffer; faceMask_:T_VkStencilFaceFlags; compareMask_:T_uint32_t ); stdcall;
type PFN_vkCmdSetStencilWriteMask                       = procedure( commandBuffer_:T_VkCommandBuffer; faceMask_:T_VkStencilFaceFlags; writeMask_:T_uint32_t ); stdcall;
type PFN_vkCmdSetStencilReference                       = procedure( commandBuffer_:T_VkCommandBuffer; faceMask_:T_VkStencilFaceFlags; reference_:T_uint32_t ); stdcall;
type PFN_vkCmdBindDescriptorSets                        = procedure( commandBuffer_:T_VkCommandBuffer; pipelineBindPoint_:T_VkPipelineBindPoint; layout_:T_VkPipelineLayout; firstSet_:T_uint32_t; descriptorSetCount_:T_uint32_t; const pDescriptorSets_:P_VkDescriptorSet; dynamicOffsetCount_:T_uint32_t; const pDynamicOffsets_:P_uint32_t ); stdcall;
type PFN_vkCmdBindIndexBuffer                           = procedure( commandBuffer_:T_VkCommandBuffer; buffer_:T_VkBuffer; offset_:T_VkDeviceSize; indexType_:T_VkIndexType ); stdcall;
type PFN_vkCmdBindVertexBuffers                         = procedure( commandBuffer_:T_VkCommandBuffer; firstBinding_:T_uint32_t; bindingCount_:T_uint32_t; const pBuffers_:P_VkBuffer; const pOffsets_:P_VkDeviceSize ); stdcall;
type PFN_vkCmdDraw                                      = procedure( commandBuffer_:T_VkCommandBuffer; vertexCount_:T_uint32_t; instanceCount_:T_uint32_t; firstVertex_:T_uint32_t; firstInstance_:T_uint32_t ); stdcall;
type PFN_vkCmdDrawIndexed                               = procedure( commandBuffer_:T_VkCommandBuffer; indexCount_:T_uint32_t; instanceCount_:T_uint32_t; firstIndex_:T_uint32_t; vertexOffset_:T_int32_t; firstInstance_:T_uint32_t ); stdcall;
type PFN_vkCmdDrawIndirect                              = procedure( commandBuffer_:T_VkCommandBuffer; buffer_:T_VkBuffer; offset_:T_VkDeviceSize; drawCount_:T_uint32_t; stride_:T_uint32_t ); stdcall;
type PFN_vkCmdDrawIndexedIndirect                       = procedure( commandBuffer_:T_VkCommandBuffer; buffer_:T_VkBuffer; offset_:T_VkDeviceSize; drawCount_:T_uint32_t; stride_:T_uint32_t ); stdcall;
type PFN_vkCmdDispatch                                  = procedure( commandBuffer_:T_VkCommandBuffer; x_:T_uint32_t; y_:T_uint32_t; z_:T_uint32_t ); stdcall;
type PFN_vkCmdDispatchIndirect                          = procedure( commandBuffer_:T_VkCommandBuffer; buffer_:T_VkBuffer; offset_:T_VkDeviceSize ); stdcall;
type PFN_vkCmdCopyBuffer                                = procedure( commandBuffer_:T_VkCommandBuffer; srcBuffer_:T_VkBuffer; dstBuffer_:T_VkBuffer; regionCount_:T_uint32_t; const pRegions_:P_VkBufferCopy ); stdcall;
type PFN_vkCmdCopyImage                                 = procedure( commandBuffer_:T_VkCommandBuffer; srcImage_:T_VkImage; srcImageLayout_:T_VkImageLayout; dstImage_:T_VkImage; dstImageLayout_:T_VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkImageCopy ); stdcall;
type PFN_vkCmdBlitImage                                 = procedure( commandBuffer_:T_VkCommandBuffer; srcImage_:T_VkImage; srcImageLayout_:T_VkImageLayout; dstImage_:T_VkImage; dstImageLayout_:T_VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkImageBlit; filter_:T_VkFilter ); stdcall;
type PFN_vkCmdCopyBufferToImage                         = procedure( commandBuffer_:T_VkCommandBuffer; srcBuffer_:T_VkBuffer; dstImage_:T_VkImage; dstImageLayout_:T_VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkBufferImageCopy ); stdcall;
type PFN_vkCmdCopyImageToBuffer                         = procedure( commandBuffer_:T_VkCommandBuffer; srcImage_:T_VkImage; srcImageLayout_:T_VkImageLayout; dstBuffer_:T_VkBuffer; regionCount_:T_uint32_t; const pRegions_:P_VkBufferImageCopy ); stdcall;
type PFN_vkCmdUpdateBuffer                              = procedure( commandBuffer_:T_VkCommandBuffer; dstBuffer_:T_VkBuffer; dstOffset_:T_VkDeviceSize; dataSize_:T_VkDeviceSize; const pData_:P_uint32_t ); stdcall;
type PFN_vkCmdFillBuffer                                = procedure( commandBuffer_:T_VkCommandBuffer; dstBuffer_:T_VkBuffer; dstOffset_:T_VkDeviceSize; size_:T_VkDeviceSize; data_:T_uint32_t ); stdcall;
type PFN_vkCmdClearColorImage                           = procedure( commandBuffer_:T_VkCommandBuffer; image_:T_VkImage; imageLayout_:T_VkImageLayout; const pColor_:P_VkClearColorValue; rangeCount_:T_uint32_t; const pRanges_:P_VkImageSubresourceRange ); stdcall;
type PFN_vkCmdClearDepthStencilImage                    = procedure( commandBuffer_:T_VkCommandBuffer; image_:T_VkImage; imageLayout_:T_VkImageLayout; const pDepthStencil_:P_VkClearDepthStencilValue; rangeCount_:T_uint32_t; const pRanges_:P_VkImageSubresourceRange ); stdcall;
type PFN_vkCmdClearAttachments                          = procedure( commandBuffer_:T_VkCommandBuffer; attachmentCount_:T_uint32_t; const pAttachments_:P_VkClearAttachment; rectCount_:T_uint32_t; const pRects_:P_VkClearRect ); stdcall;
type PFN_vkCmdResolveImage                              = procedure( commandBuffer_:T_VkCommandBuffer; srcImage_:T_VkImage; srcImageLayout_:T_VkImageLayout; dstImage_:T_VkImage; dstImageLayout_:T_VkImageLayout; regionCount_:T_uint32_t; const pRegions_:P_VkImageResolve ); stdcall;
type PFN_vkCmdSetEvent                                  = procedure( commandBuffer_:T_VkCommandBuffer; event_:T_VkEvent; stageMask_:T_VkPipelineStageFlags ); stdcall;
type PFN_vkCmdResetEvent                                = procedure( commandBuffer_:T_VkCommandBuffer; event_:T_VkEvent; stageMask_:T_VkPipelineStageFlags ); stdcall;
type PFN_vkCmdWaitEvents                                = procedure( commandBuffer_:T_VkCommandBuffer; eventCount_:T_uint32_t; const pEvents_:P_VkEvent; srcStageMask_:T_VkPipelineStageFlags; dstStageMask_:T_VkPipelineStageFlags; memoryBarrierCount_:T_uint32_t; const pMemoryBarriers_:P_VkMemoryBarrier; bufferMemoryBarrierCount_:T_uint32_t; const pBufferMemoryBarriers_:P_VkBufferMemoryBarrier; imageMemoryBarrierCount_:T_uint32_t; const pImageMemoryBarriers_:P_VkImageMemoryBarrier ); stdcall;
type PFN_vkCmdPipelineBarrier                           = procedure( commandBuffer_:T_VkCommandBuffer; srcStageMask_:T_VkPipelineStageFlags; dstStageMask_:T_VkPipelineStageFlags; dependencyFlags_:T_VkDependencyFlags; memoryBarrierCount_:T_uint32_t; const pMemoryBarriers_:P_VkMemoryBarrier; bufferMemoryBarrierCount_:T_uint32_t; const pBufferMemoryBarriers_:P_VkBufferMemoryBarrier; imageMemoryBarrierCount_:T_uint32_t; const pImageMemoryBarriers_:P_VkImageMemoryBarrier ); stdcall;
type PFN_vkCmdBeginQuery                                = procedure( commandBuffer_:T_VkCommandBuffer; queryPool_:T_VkQueryPool; query_:T_uint32_t; flags_:T_VkQueryControlFlags ); stdcall;
type PFN_vkCmdEndQuery                                  = procedure( commandBuffer_:T_VkCommandBuffer; queryPool_:T_VkQueryPool; query_:T_uint32_t ); stdcall;
type PFN_vkCmdResetQueryPool                            = procedure( commandBuffer_:T_VkCommandBuffer; queryPool_:T_VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t ); stdcall;
type PFN_vkCmdWriteTimestamp                            = procedure( commandBuffer_:T_VkCommandBuffer; pipelineStage_:T_VkPipelineStageFlagBits; queryPool_:T_VkQueryPool; query_:T_uint32_t ); stdcall;
type PFN_vkCmdCopyQueryPoolResults                      = procedure( commandBuffer_:T_VkCommandBuffer; queryPool_:T_VkQueryPool; firstQuery_:T_uint32_t; queryCount_:T_uint32_t; dstBuffer_:T_VkBuffer; dstOffset_:T_VkDeviceSize; stride_:T_VkDeviceSize; flags_:T_VkQueryResultFlags ); stdcall;
type PFN_vkCmdPushConstants                             = procedure( commandBuffer_:T_VkCommandBuffer; layout_:T_VkPipelineLayout; stageFlags_:T_VkShaderStageFlags; offset_:T_uint32_t; size_:T_uint32_t; const pValues_:P_void ); stdcall;
type PFN_vkCmdBeginRenderPass                           = procedure( commandBuffer_:T_VkCommandBuffer; const pRenderPassBegin_:P_VkRenderPassBeginInfo; contents_:T_VkSubpassContents ); stdcall;
type PFN_vkCmdNextSubpass                               = procedure( commandBuffer_:T_VkCommandBuffer; contents_:T_VkSubpassContents ); stdcall;
type PFN_vkCmdEndRenderPass                             = procedure( commandBuffer_:T_VkCommandBuffer ); stdcall;
type PFN_vkCmdExecuteCommands                           = procedure( commandBuffer_:T_VkCommandBuffer; commandBufferCount_:T_uint32_t; const pCommandBuffers_:P_VkCommandBuffer ); stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateInstance(
  const  pCreateInfo_ :P_VkInstanceCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pInstance_   :P_VkInstance            ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyInstance(
          instance_  :T_VkInstance;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkEnumeratePhysicalDevices(
          instance_            :T_VkInstance;
         pPhysicalDeviceCount_ :P_uint32_t;
         pPhysicalDevices_     :P_VkPhysicalDevice ) :T_VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFeatures(
          physicalDevice_ :T_VkPhysicalDevice;
         pFeatures_       :P_VkPhysicalDeviceFeatures ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceFormatProperties(
          physicalDevice_   :T_VkPhysicalDevice;
          format_           :T_VkFormat;
         pFormatProperties_ :P_VkFormatProperties ); stdcall; external DLLNAME;

function vkGetPhysicalDeviceImageFormatProperties(
          physicalDevice_        :T_VkPhysicalDevice;
          format_                :T_VkFormat;
          type_                  :T_VkImageType;
          tiling_                :T_VkImageTiling;
          usage_                 :T_VkImageUsageFlags;
          flags_                 :T_VkImageCreateFlags;
         pImageFormatProperties_ :P_VkImageFormatProperties ) :T_VkResult; stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceProperties(
          physicalDevice_ :T_VkPhysicalDevice;
         pProperties_     :P_VkPhysicalDeviceProperties ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceQueueFamilyProperties(
          physicalDevice_           :T_VkPhysicalDevice;
         pQueueFamilyPropertyCount_ :P_uint32_t;
         pQueueFamilyProperties_    :P_VkQueueFamilyProperties ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceMemoryProperties(
          physicalDevice_   :T_VkPhysicalDevice;
         pMemoryProperties_ :P_VkPhysicalDeviceMemoryProperties ); stdcall; external DLLNAME;

function vkGetInstanceProcAddr(
          instance_ :T_VkInstance;
  const  pName_     :P_char    ) :PFN_vkVoidFunction; stdcall; external DLLNAME;

function vkGetDeviceProcAddr(
          device_ :T_VkDevice;
  const  pName_   :P_char  ) :PFN_vkVoidFunction; stdcall; external DLLNAME;

function vkCreateDevice(
          physicalDevice_ :T_VkPhysicalDevice;
  const  pCreateInfo_     :P_VkDeviceCreateInfo;
  const  pAllocator_      :P_VkAllocationCallbacks;
         pDevice_         :P_VkDevice              ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyDevice(
          device_    :T_VkDevice;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkEnumerateInstanceExtensionProperties(
  const  pLayerName_     :P_char;
         pPropertyCount_ :P_uint32_t;
         pProperties_    :P_VkExtensionProperties ) :T_VkResult; stdcall; external DLLNAME;

function vkEnumerateDeviceExtensionProperties(
          physicalDevice_ :T_VkPhysicalDevice;
  const  pLayerName_      :P_char;
         pPropertyCount_  :P_uint32_t;
         pProperties_     :P_VkExtensionProperties ) :T_VkResult; stdcall; external DLLNAME;

function vkEnumerateInstanceLayerProperties(
         pPropertyCount_ :P_uint32_t;
         pProperties_    :P_VkLayerProperties ) :T_VkResult; stdcall; external DLLNAME;

function vkEnumerateDeviceLayerProperties(
          physicalDevice_ :T_VkPhysicalDevice;
         pPropertyCount_  :P_uint32_t;
         pProperties_     :P_VkLayerProperties ) :T_VkResult; stdcall; external DLLNAME;

procedure vkGetDeviceQueue(
          device_           :T_VkDevice;
          queueFamilyIndex_ :T_uint32_t;
          queueIndex_       :T_uint32_t;
         pQueue_            :P_VkQueue  ); stdcall; external DLLNAME;

function vkQueueSubmit(
          queue_       :T_VkQueue;
          submitCount_ :T_uint32_t;
  const  pSubmits_     :P_VkSubmitInfo;
          fence_       :T_VkFence      ) :T_VkResult; stdcall; external DLLNAME;

function vkQueueWaitIdle(
          queue_ :T_VkQueue ) :T_VkResult; stdcall; external DLLNAME;

function vkDeviceWaitIdle(
          device_ :T_VkDevice ) :T_VkResult; stdcall; external DLLNAME;

function vkAllocateMemory(
          device_       :T_VkDevice;
  const  pAllocateInfo_ :P_VkMemoryAllocateInfo;
  const  pAllocator_    :P_VkAllocationCallbacks;
         pMemory_       :P_VkDeviceMemory        ) :T_VkResult; stdcall; external DLLNAME;

procedure vkFreeMemory(
          device_    :T_VkDevice;
          memory_    :T_VkDeviceMemory;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkMapMemory(
          device_ :T_VkDevice;
          memory_ :T_VkDeviceMemory;
          offset_ :T_VkDeviceSize;
          size_   :T_VkDeviceSize;
          flags_  :T_VkMemoryMapFlags;
        ppData_   :PP_void            ) :T_VkResult; stdcall; external DLLNAME;

procedure vkUnmapMemory(
          device_ :T_VkDevice;
          memory_ :T_VkDeviceMemory ); stdcall; external DLLNAME;

function vkFlushMappedMemoryRanges(
          device_           :T_VkDevice;
          memoryRangeCount_ :T_uint32_t;
  const  pMemoryRanges_     :P_VkMappedMemoryRange ) :T_VkResult; stdcall; external DLLNAME;

function vkInvalidateMappedMemoryRanges(
          device_           :T_VkDevice;
          memoryRangeCount_ :T_uint32_t;
  const  pMemoryRanges_     :P_VkMappedMemoryRange ) :T_VkResult; stdcall; external DLLNAME;

procedure vkGetDeviceMemoryCommitment(
          device_                 :T_VkDevice;
          memory_                 :T_VkDeviceMemory;
         pCommittedMemoryInBytes_ :P_VkDeviceSize   ); stdcall; external DLLNAME;

function vkBindBufferMemory(
          device_       :T_VkDevice;
          buffer_       :T_VkBuffer;
          memory_       :T_VkDeviceMemory;
          memoryOffset_ :T_VkDeviceSize   ) :T_VkResult; stdcall; external DLLNAME;

function vkBindImageMemory(
          device_       :T_VkDevice;
          image_        :T_VkImage;
          memory_       :T_VkDeviceMemory;
          memoryOffset_ :T_VkDeviceSize   ) :T_VkResult; stdcall; external DLLNAME;

procedure vkGetBufferMemoryRequirements(
          device_             :T_VkDevice;
          buffer_             :T_VkBuffer;
         pMemoryRequirements_ :P_VkMemoryRequirements ); stdcall; external DLLNAME;

procedure vkGetImageMemoryRequirements(
          device_             :T_VkDevice;
          image_              :T_VkImage;
         pMemoryRequirements_ :P_VkMemoryRequirements ); stdcall; external DLLNAME;

procedure vkGetImageSparseMemoryRequirements(
          device_                       :T_VkDevice;
          image_                        :T_VkImage;
         pSparseMemoryRequirementCount_ :P_uint32_t;
         pSparseMemoryRequirements_     :P_VkSparseImageMemoryRequirements ); stdcall; external DLLNAME;

procedure vkGetPhysicalDeviceSparseImageFormatProperties(
          physicalDevice_ :T_VkPhysicalDevice;
          format_         :T_VkFormat;
          type_           :T_VkImageType;
          samples_        :T_VkSampleCountFlagBits;
          usage_          :T_VkImageUsageFlags;
          tiling_         :T_VkImageTiling;
         pPropertyCount_  :P_uint32_t;
         pProperties_     :P_VkSparseImageFormatProperties ); stdcall; external DLLNAME;

function vkQueueBindSparse(
          queue_         :T_VkQueue;
          bindInfoCount_ :T_uint32_t;
  const  pBindInfo_      :P_VkBindSparseInfo;
          fence_         :T_VkFence          ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateFence(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkFenceCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pFence_      :P_VkFence               ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyFence(
          device_    :T_VkDevice;
          fence_     :T_VkFence;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkResetFences(
          device_     :T_VkDevice;
          fenceCount_ :T_uint32_t;
  const  pFences_     :P_VkFence  ) :T_VkResult; stdcall; external DLLNAME;

function vkGetFenceStatus(
          device_     :T_VkDevice;
          fence_      :T_VkFence  ) :T_VkResult; stdcall; external DLLNAME;

function vkWaitForFences(
          device_     :T_VkDevice;
          fenceCount_ :T_uint32_t;
  const  pFences_     :P_VkFence;
          waitAll_    :T_VkBool32;
          timeout_    :T_uint64_t ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateSemaphore(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkSemaphoreCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSemaphore_  :P_VkSemaphore           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroySemaphore(
          device_ :T_VkDevice;
          semaphore_ :T_VkSemaphore;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateEvent(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkEventCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pEvent_      :P_VkEvent               ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyEvent(
          device_    :T_VkDevice;
          event_     :T_VkEvent;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetEventStatus(
          device_ :T_VkDevice;
          event_  :T_VkEvent  ) :T_VkResult; stdcall; external DLLNAME;

function vkSetEvent(
          device_ :T_VkDevice;
          event_  :T_VkEvent  ) :T_VkResult; stdcall; external DLLNAME;

function vkResetEvent(
          device_ :T_VkDevice;
          event_  :T_VkEvent  ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateQueryPool(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkQueryPoolCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pQueryPool_  :P_VkQueryPool           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyQueryPool(
          device_    :T_VkDevice;
          queryPool_ :T_VkQueryPool;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetQueryPoolResults(
          device_     :T_VkDevice;
          queryPool_  :T_VkQueryPool;
          firstQuery_ :T_uint32_t;
          queryCount_ :T_uint32_t;
          dataSize_   :T_size_t;
         pData_       :P_void;
          stride_     :T_VkDeviceSize;
          flags_      :T_VkQueryResultFlags ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateBuffer(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkBufferCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pBuffer_     :P_VkBuffer              ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyBuffer(
          device_    :T_VkDevice;
          buffer_    :T_VkBuffer;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateBufferView(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkBufferViewCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pView_       :P_VkBufferView           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyBufferView(
          device_     :T_VkDevice;
          bufferView_ :T_VkBufferView;
  const  pAllocator_  :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateImage(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkImageCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pImage_      :P_VkImage               ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyImage(
          device_     :T_VkDevice;
          image_      :T_VkImage;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkGetImageSubresourceLayout(
          device_      :T_VkDevice;
          image_       :T_VkImage;
  const  pSubresource_ :P_VkImageSubresource;
         pLayout_      :P_VkSubresourceLayout ); stdcall; external DLLNAME;

function vkCreateImageView(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkImageViewCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pView_       :P_VkImageView           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyImageView(
          device_    :T_VkDevice;
          imageView_ :T_VkImageView;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateShaderModule(
          device_       :T_VkDevice;
  const  pCreateInfo_   :P_VkShaderModuleCreateInfo;
  const  pAllocator_    :P_VkAllocationCallbacks;
         pShaderModule_ :P_VkShaderModule           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyShaderModule(
          device_       :T_VkDevice;
          shaderModule_ :T_VkShaderModule;
  const  pAllocator_    :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreatePipelineCache(
          device_        :T_VkDevice;
  const  pCreateInfo_    :P_VkPipelineCacheCreateInfo;
  const  pAllocator_     :P_VkAllocationCallbacks;
         pPipelineCache_ :P_VkPipelineCache           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyPipelineCache(
          device_        :T_VkDevice;
          pipelineCache_ :T_VkPipelineCache;
  const  pAllocator_     :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetPipelineCacheData(
          device_        :T_VkDevice;
          pipelineCache_ :T_VkPipelineCache;
         pDataSize_      :P_size_t;
         pData_          :P_void            ) :T_VkResult; stdcall; external DLLNAME;

function vkMergePipelineCaches(
          device_        :T_VkDevice;
          dstCache_      :T_VkPipelineCache;
          srcCacheCount_ :T_uint32_t;
  const  pSrcCaches_     :P_VkPipelineCache ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateGraphicsPipelines(
          device_          :T_VkDevice;
          pipelineCache_   :T_VkPipelineCache;
          createInfoCount_ :T_uint32_t;
  const  pCreateInfos_     :P_VkGraphicsPipelineCreateInfo;
  const  pAllocator_       :P_VkAllocationCallbacks;
         pPipelines_       :P_VkPipeline                   ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateComputePipelines(
          device_           :T_VkDevice;
          pipelineCache_    :T_VkPipelineCache;
          createInfoCount_ :T_uint32_t;
  const  pCreateInfos_     :P_VkComputePipelineCreateInfo;
  const  pAllocator_       :P_VkAllocationCallbacks;
         pPipelines_       :P_VkPipeline                  ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyPipeline(
          device_    :T_VkDevice;
          pipeline_  :T_VkPipeline;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreatePipelineLayout(
          device_         :T_VkDevice;
  const  pCreateInfo_     :P_VkPipelineLayoutCreateInfo;
  const  pAllocator_      :P_VkAllocationCallbacks;
         pPipelineLayout_ :P_VkPipelineLayout           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyPipelineLayout(
          device_         :T_VkDevice;
          pipelineLayout_ :T_VkPipelineLayout;
  const  pAllocator_      :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateSampler(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkSamplerCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSampler_    :P_VkSampler             ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroySampler(
          device_    :T_VkDevice;
          sampler_   :T_VkSampler;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateDescriptorSetLayout(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkDescriptorSetLayoutCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSetLayout_  :P_VkDescriptorSetLayout           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyDescriptorSetLayout(
          device_              :T_VkDevice;
          descriptorSetLayout_ :T_VkDescriptorSetLayout;
  const  pAllocator_           :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateDescriptorPool(
          device_         :T_VkDevice;
  const  pCreateInfo_     :P_VkDescriptorPoolCreateInfo;
  const  pAllocator_      :P_VkAllocationCallbacks;
         pDescriptorPool_ :P_VkDescriptorPool           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyDescriptorPool(
          device_         :T_VkDevice;
          descriptorPool_ :T_VkDescriptorPool;
  const  pAllocator_      :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkResetDescriptorPool(
          device_         :T_VkDevice;
          descriptorPool_ :T_VkDescriptorPool;
          flags_          :T_VkDescriptorPoolResetFlags ) :T_VkResult; stdcall; external DLLNAME;

function vkAllocateDescriptorSets(
          device_         :T_VkDevice;
  const  pAllocateInfo_   :P_VkDescriptorSetAllocateInfo;
         pDescriptorSets_ :P_VkDescriptorSet             ) :T_VkResult; stdcall; external DLLNAME;

function vkFreeDescriptorSets(
          device_             :T_VkDevice;
          descriptorPool_     :T_VkDescriptorPool;
          descriptorSetCount_ :T_uint32_t;
  const  pDescriptorSets_     :P_VkDescriptorSet  ) :T_VkResult; stdcall; external DLLNAME;

procedure vkUpdateDescriptorSets(
          device_               :T_VkDevice;
          descriptorWriteCount_ :T_uint32_t;
  const  pDescriptorWrites_     :P_VkWriteDescriptorSet;
          descriptorCopyCount_  :T_uint32_t;
  const  pDescriptorCopies_     :P_VkCopyDescriptorSet  ); stdcall; external DLLNAME;

function vkCreateFramebuffer(
          device_      :T_VkDevice;
  const  pCreateInfo_  :P_VkFramebufferCreateInfo;
  const  pAllocator_   :P_VkAllocationCallbacks;
         pFramebuffer_ :P_VkFramebuffer           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyFramebuffer(
          device_      :T_VkDevice;
          framebuffer_ :T_VkFramebuffer;
  const  pAllocator_   :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkCreateRenderPass(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkRenderPassCreateInfo;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pRenderPass_ :P_VkRenderPass          ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyRenderPass(
          device_     :T_VkDevice;
          renderPass_ :T_VkRenderPass;
  const  pAllocator_  :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

procedure vkGetRenderAreaGranularity(
          device_      :T_VkDevice;
          renderPass_  :T_VkRenderPass;
         pGranularity_ :P_VkExtent2D   ); stdcall; external DLLNAME;

function vkCreateCommandPool(
          device_      :T_VkDevice;
  const  pCreateInfo_  :P_VkCommandPoolCreateInfo;
  const  pAllocator_   :P_VkAllocationCallbacks;
         pCommandPool_ :P_VkCommandPool           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyCommandPool(
          device_      :T_VkDevice;
          commandPool_ :T_VkCommandPool;
  const  pAllocator_   :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkResetCommandPool(
          device_      :T_VkDevice;
          commandPool_ :T_VkCommandPool;
          flags_       :T_VkCommandPoolResetFlags ) :T_VkResult; stdcall; external DLLNAME;

function vkAllocateCommandBuffers(
          device_         :T_VkDevice;
  const  pAllocateInfo_   :P_VkCommandBufferAllocateInfo;
         pCommandBuffers_ :P_VkCommandBuffer             ) :T_VkResult; stdcall; external DLLNAME;

procedure vkFreeCommandBuffers(
          device_             :T_VkDevice;
          commandPool_        :T_VkCommandPool;
          commandBufferCount_ :T_uint32_t;
  const  pCommandBuffers_     :P_VkCommandBuffer ); stdcall; external DLLNAME;

function vkBeginCommandBuffer(
          commandBuffer_ :T_VkCommandBuffer;
  const  pBeginInfo_     :P_VkCommandBufferBeginInfo ) :T_VkResult; stdcall; external DLLNAME;

function vkEndCommandBuffer(
          commandBuffer_ :T_VkCommandBuffer ) :T_VkResult; stdcall; external DLLNAME;

function vkResetCommandBuffer(
          commandBuffer_ :T_VkCommandBuffer;
          flags_         :T_VkCommandBufferResetFlags ) :T_VkResult; stdcall; external DLLNAME;

procedure vkCmdBindPipeline(
          commandBuffer_     :T_VkCommandBuffer;
          pipelineBindPoint_ :T_VkPipelineBindPoint;
          pipeline_          :T_VkPipeline          ); stdcall; external DLLNAME;

procedure vkCmdSetViewport(
          commandBuffer_ :T_VkCommandBuffer;
          firstViewport_ :T_uint32_t;
          viewportCount_ :T_uint32_t;
  const  pViewports_     :P_VkViewport      ); stdcall; external DLLNAME;

procedure vkCmdSetScissor(
          commandBuffer_ :T_VkCommandBuffer;
          firstScissor_  :T_uint32_t;
          scissorCount_  :T_uint32_t;
  const  pScissors_      :P_VkRect2D        ); stdcall; external DLLNAME;

procedure vkCmdSetLineWidth(
          commandBuffer_ :T_VkCommandBuffer;
          lineWidth_     :T_float           ); stdcall; external DLLNAME;

procedure vkCmdSetDepthBias(
          commandBuffer_           :T_VkCommandBuffer;
          depthBiasConstantFactor_ :T_float;
          depthBiasClamp_          :T_float;
          depthBiasSlopeFactor_    :T_float           ); stdcall; external DLLNAME;

procedure vkCmdSetBlendConstants(
          commandBuffer_ :T_VkCommandBuffer;
  const blendConstants_  :T_float4          ); stdcall; external DLLNAME;

procedure vkCmdSetDepthBounds(
          commandBuffer_  :T_VkCommandBuffer;
          minDepthBounds_ :T_float;
          maxDepthBounds_ :T_float           ); stdcall; external DLLNAME;

procedure vkCmdSetStencilCompareMask(
          commandBuffer_ :T_VkCommandBuffer;
          faceMask_      :T_VkStencilFaceFlags;
          compareMask_   :T_uint32_t           ); stdcall; external DLLNAME;

procedure vkCmdSetStencilWriteMask(
          commandBuffer_ :T_VkCommandBuffer;
          faceMask_      :T_VkStencilFaceFlags;
          writeMask_     :T_uint32_t           ); stdcall; external DLLNAME;

procedure vkCmdSetStencilReference(
          commandBuffer_ :T_VkCommandBuffer;
          faceMask_      :T_VkStencilFaceFlags;
          reference_     :T_uint32_t           ); stdcall; external DLLNAME;

procedure vkCmdBindDescriptorSets(
          commandBuffer_      :T_VkCommandBuffer;
          pipelineBindPoint_  :T_VkPipelineBindPoint;
          layout_             :T_VkPipelineLayout;
          firstSet_           :T_uint32_t;
          descriptorSetCount_ :T_uint32_t;
  const  pDescriptorSets_     :P_VkDescriptorSet;
          dynamicOffsetCount_ :T_uint32_t;
  const  pDynamicOffsets_     :P_uint32_t            ); stdcall; external DLLNAME;

procedure vkCmdBindIndexBuffer(
          commandBuffer_ :T_VkCommandBuffer;
          buffer_        :T_VkBuffer;
          offset_        :T_VkDeviceSize;
          indexType_     :T_VkIndexType     ); stdcall; external DLLNAME;

procedure vkCmdBindVertexBuffers(
          commandBuffer_ :T_VkCommandBuffer;
          firstBinding_  :T_uint32_t;
          bindingCount_  :T_uint32_t;
  const  pBuffers_       :P_VkBuffer;
  const  pOffsets_       :P_VkDeviceSize    ); stdcall; external DLLNAME;

procedure vkCmdDraw(
          commandBuffer_ :T_VkCommandBuffer;
          vertexCount_   :T_uint32_t;
          instanceCount_ :T_uint32_t;
          firstVertex_   :T_uint32_t;
          firstInstance_ :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexed(
          commandBuffer_ :T_VkCommandBuffer;
          indexCount_    :T_uint32_t;
          instanceCount_ :T_uint32_t;
          firstIndex_    :T_uint32_t;
          vertexOffset_  :T_int32_t;
          firstInstance_ :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdDrawIndirect(
          commandBuffer_ :T_VkCommandBuffer;
          buffer_        :T_VkBuffer;
          offset_        :T_VkDeviceSize;
          drawCount_     :T_uint32_t;
          stride_         :T_uint32_t       ); stdcall; external DLLNAME;

procedure vkCmdDrawIndexedIndirect(
          commandBuffer_ :T_VkCommandBuffer;
          buffer_        :T_VkBuffer;
          offset_        :T_VkDeviceSize;
          drawCount_     :T_uint32_t;
          stride_        :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdDispatch(
          commandBuffer_ :T_VkCommandBuffer;
          x_             :T_uint32_t;
          y_             :T_uint32_t;
          z_             :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdDispatchIndirect(
          commandBuffer_ :T_VkCommandBuffer;
          buffer_        :T_VkBuffer;
          offset_        :T_VkDeviceSize    ); stdcall; external DLLNAME;

procedure vkCmdCopyBuffer(
          commandBuffer_ :T_VkCommandBuffer;
          srcBuffer_     :T_VkBuffer;
          dstBuffer_     :T_VkBuffer;
          regionCount_   :T_uint32_t;
  const  pRegions_       :P_VkBufferCopy    ); stdcall; external DLLNAME;

procedure vkCmdCopyImage(
          commandBuffer_  :T_VkCommandBuffer;
          srcImage_       :T_VkImage;
          srcImageLayout_ :T_VkImageLayout;
          dstImage_       :T_VkImage;
          dstImageLayout_ :T_VkImageLayout;
          regionCount_    :T_uint32_t;
  const  pRegions_        :P_VkImageCopy     ); stdcall; external DLLNAME;

procedure vkCmdBlitImage(
          commandBuffer_  :T_VkCommandBuffer;
          srcImage_       :T_VkImage;
          srcImageLayout_ :T_VkImageLayout;
          dstImage_       :T_VkImage;
          dstImageLayout_ :T_VkImageLayout;
          regionCount_    :T_uint32_t;
  const  pRegions_        :P_VkImageBlit;
          filter_         :T_VkFilter      ); stdcall; external DLLNAME;

procedure vkCmdCopyBufferToImage(
          commandBuffer_  :T_VkCommandBuffer;
          srcBuffer_      :T_VkBuffer;
          dstImage_       :T_VkImage;
          dstImageLayout_ :T_VkImageLayout;
          regionCount_    :T_uint32_t;
  const  pRegions_        :P_VkBufferImageCopy );stdcall; external DLLNAME;

procedure vkCmdCopyImageToBuffer(
          commandBuffer_ :T_VkCommandBuffer;
          srcImage_ :T_VkImage;
          srcImageLayout_ :T_VkImageLayout;
          dstBuffer_ :T_VkBuffer;
          regionCount_ :T_uint32_t;
  const  pRegions_ :P_VkBufferImageCopy     );stdcall; external DLLNAME;

procedure vkCmdUpdateBuffer(
          commandBuffer_ :T_VkCommandBuffer;
          dstBuffer_     :T_VkBuffer;
          dstOffset_     :T_VkDeviceSize;
          dataSize_      :T_VkDeviceSize;
  const  pData_          :P_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdFillBuffer(
          commandBuffer_ :T_VkCommandBuffer;
          dstBuffer_     :T_VkBuffer;
          dstOffset_     :T_VkDeviceSize;
          size_          :T_VkDeviceSize;
          data_          :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdClearColorImage(
          commandBuffer_ :T_VkCommandBuffer;
          image_         :T_VkImage;
          imageLayout_   :T_VkImageLayout;
  const  pColor_         :P_VkClearColorValue;
          rangeCount_    :T_uint32_t;
  const  pRanges_        :P_VkImageSubresourceRange ); stdcall; external DLLNAME;

procedure vkCmdClearDepthStencilImage(
          commandBuffer_ :T_VkCommandBuffer;
          image_         :T_VkImage;
          imageLayout_   :T_VkImageLayout;
  const  pDepthStencil_  :P_VkClearDepthStencilValue;
          rangeCount_    :T_uint32_t;
  const  pRanges_        :P_VkImageSubresourceRange  ); stdcall; external DLLNAME;

procedure vkCmdClearAttachments(
          commandBuffer_   :T_VkCommandBuffer;
          attachmentCount_ :T_uint32_t;
  const  pAttachments_     :P_VkClearAttachment;
          rectCount_       :T_uint32_t;
  const  pRects_           :P_VkClearRect       ); stdcall; external DLLNAME;

procedure vkCmdResolveImage(
          commandBuffer_  :T_VkCommandBuffer;
          srcImage_       :T_VkImage;
          srcImageLayout_ :T_VkImageLayout;
          dstImage_       :T_VkImage;
          dstImageLayout_ :T_VkImageLayout;
          regionCount_    :T_uint32_t;
  const  pRegions_        :P_VkImageResolve  ); stdcall; external DLLNAME;

procedure vkCmdSetEvent(
          commandBuffer_ :T_VkCommandBuffer;
          event_         :T_VkEvent;
          stageMask_     :T_VkPipelineStageFlags ); stdcall; external DLLNAME;

procedure vkCmdResetEvent(
          commandBuffer_ :T_VkCommandBuffer;
          event_         :T_VkEvent;
          stageMask_     :T_VkPipelineStageFlags ); stdcall; external DLLNAME;

procedure vkCmdWaitEvents(
          commandBuffer_            :T_VkCommandBuffer;
          eventCount_               :T_uint32_t;
  const  pEvents_                   :P_VkEvent;
          srcStageMask_             :T_VkPipelineStageFlags;
          dstStageMask_             :T_VkPipelineStageFlags;
          memoryBarrierCount_       :T_uint32_t;
  const  pMemoryBarriers_           :P_VkMemoryBarrier;
          bufferMemoryBarrierCount_ :T_uint32_t;
  const  pBufferMemoryBarriers_     :P_VkBufferMemoryBarrier;
          imageMemoryBarrierCount_  :T_uint32_t;
  const  pImageMemoryBarriers_      :P_VkImageMemoryBarrier  ); stdcall; external DLLNAME;

procedure vkCmdPipelineBarrier(
          commandBuffer_            :T_VkCommandBuffer;
          srcStageMask_             :T_VkPipelineStageFlags;
          dstStageMask_             :T_VkPipelineStageFlags;
          dependencyFlags_          :T_VkDependencyFlags;
          memoryBarrierCount_       :T_uint32_t;
  const  pMemoryBarriers_           :P_VkMemoryBarrier;
          bufferMemoryBarrierCount_ :T_uint32_t;
  const  pBufferMemoryBarriers_     :P_VkBufferMemoryBarrier;
          imageMemoryBarrierCount_  :T_uint32_t;
  const  pImageMemoryBarriers_      :P_VkImageMemoryBarrier  ); stdcall; external DLLNAME;

procedure vkCmdBeginQuery(
          commandBuffer_ :T_VkCommandBuffer;
          queryPool_     :T_VkQueryPool;
          query_         :T_uint32_t;
          flags_         :T_VkQueryControlFlags ); stdcall; external DLLNAME;

procedure vkCmdEndQuery(
          commandBuffer_ :T_VkCommandBuffer;
          queryPool_     :T_VkQueryPool;
          query_         :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdResetQueryPool(
          commandBuffer_ :T_VkCommandBuffer;
          queryPool_     :T_VkQueryPool;
          firstQuery_    :T_uint32_t;
          queryCount_    :T_uint32_t        ); stdcall; external DLLNAME;

procedure vkCmdWriteTimestamp(
          commandBuffer_ :T_VkCommandBuffer;
          pipelineStage_ :T_VkPipelineStageFlagBits;
          queryPool_     :T_VkQueryPool;
          query_         :T_uint32_t                ); stdcall; external DLLNAME;

procedure vkCmdCopyQueryPoolResults(
          commandBuffer_ :T_VkCommandBuffer;
          queryPool_     :T_VkQueryPool;
          firstQuery_    :T_uint32_t;
          queryCount_    :T_uint32_t;
          dstBuffer_     :T_VkBuffer;
          dstOffset_     :T_VkDeviceSize;
          stride_        :T_VkDeviceSize;
          flags_         :T_VkQueryResultFlags ); stdcall; external DLLNAME;

procedure vkCmdPushConstants(
          commandBuffer_ :T_VkCommandBuffer;
          layout_        :T_VkPipelineLayout;
          stageFlags_    :T_VkShaderStageFlags;
          offset_        :T_uint32_t;
          size_          :T_uint32_t;
  const  pValues_        :P_void               ); stdcall; external DLLNAME;

procedure vkCmdBeginRenderPass(
          commandBuffer_   :T_VkCommandBuffer;
  const  pRenderPassBegin_ :P_VkRenderPassBeginInfo;
          contents_        :T_VkSubpassContents     ); stdcall; external DLLNAME;

procedure vkCmdNextSubpass(
          commandBuffer_ :T_VkCommandBuffer;
          contents_      :T_VkSubpassContents ); stdcall; external DLLNAME;

procedure vkCmdEndRenderPass(
          commandBuffer_ :T_VkCommandBuffer ); stdcall; external DLLNAME;

procedure vkCmdExecuteCommands(
          commandBuffer_      :T_VkCommandBuffer;
          commandBufferCount_ :T_uint32_t;
  const  pCommandBuffers_     :P_VkCommandBuffer ); stdcall; external DLLNAME;
{$ENDIF}

const VK_KHR_surface = 1;
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or Defined( __x86_64__ ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
type T_VkSurfaceKHR = ^VkSurfaceKHR_T; VkSurfaceKHR_T = record end;
{$ELSE}
type T_VkSurfaceKHR = T_uint64_t;
{$ENDIF}
type P_VkSurfaceKHR = ^T_VkSurfaceKHR;

const VK_KHR_SURFACE_SPEC_VERSION   = 25;
const VK_KHR_SURFACE_EXTENSION_NAME = 'VK_KHR_surface';

type P_VkColorSpaceKHR = ^T_VkColorSpaceKHR;
     T_VkColorSpaceKHR = (
       VK_COLORSPACE_SRGB_NONLINEAR_KHR = 0,
       VK_COLORSPACE_BEGIN_RANGE        = VK_COLORSPACE_SRGB_NONLINEAR_KHR,
       VK_COLORSPACE_END_RANGE          = VK_COLORSPACE_SRGB_NONLINEAR_KHR,
       VK_COLORSPACE_RANGE_SIZE         = ( VK_COLORSPACE_SRGB_NONLINEAR_KHR - VK_COLORSPACE_SRGB_NONLINEAR_KHR + 1 ),
       VK_COLORSPACE_MAX_ENUM           = $7FFFFFFF
     );

type P_VkPresentModeKHR = ^T_VkPresentModeKHR;
     T_VkPresentModeKHR = (
       VK_PRESENT_MODE_IMMEDIATE_KHR    = 0,
       VK_PRESENT_MODE_MAILBOX_KHR      = 1,
       VK_PRESENT_MODE_FIFO_KHR         = 2,
       VK_PRESENT_MODE_FIFO_RELAXED_KHR = 3,
       VK_PRESENT_MODE_BEGIN_RANGE      = VK_PRESENT_MODE_IMMEDIATE_KHR,
       VK_PRESENT_MODE_END_RANGE        = VK_PRESENT_MODE_FIFO_RELAXED_KHR,
       VK_PRESENT_MODE_RANGE_SIZE       = ( VK_PRESENT_MODE_FIFO_RELAXED_KHR - VK_PRESENT_MODE_IMMEDIATE_KHR + 1 ),
       VK_PRESENT_MODE_MAX_ENUM         = $7FFFFFFF
     );

type P_VkSurfaceTransformFlagBitsKHR = ^T_VkSurfaceTransformFlagBitsKHR;
     T_VkSurfaceTransformFlagBitsKHR = (
       VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR                     = $00000001,
       VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                    = $00000002,
       VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                   = $00000004,
       VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                   = $00000008,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR            = $00000010,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR  = $00000020,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = $00000040,
       VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = $00000080,
       VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR                      = $00000100
     );
type T_VkSurfaceTransformFlagsKHR = T_VkFlags;

type P_VkCompositeAlphaFlagBitsKHR = ^T_VkCompositeAlphaFlagBitsKHR;
     T_VkCompositeAlphaFlagBitsKHR = (
       VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR          = $00000001,
       VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  = $00000002,
       VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = $00000004,
       VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR         = $00000008
     );
type T_VkCompositeAlphaFlagsKHR = T_VkFlags;

type P_VkSurfaceCapabilitiesKHR = ^T_VkSurfaceCapabilitiesKHR;
     T_VkSurfaceCapabilitiesKHR = record
          minImageCount           :T_uint32_t;
          maxImageCount           :T_uint32_t;
          currentExtent           :T_VkExtent2D;
          minImageExtent          :T_VkExtent2D;
          maxImageExtent          :T_VkExtent2D;
          maxImageArrayLayers     :T_uint32_t;
          supportedTransforms     :T_VkSurfaceTransformFlagsKHR;
          currentTransform        :T_VkSurfaceTransformFlagBitsKHR;
          supportedCompositeAlpha :T_VkCompositeAlphaFlagsKHR;
          supportedUsageFlags     :T_VkImageUsageFlags;
     end;

type P_VkSurfaceFormatKHR = ^T_VkSurfaceFormatKHR;
     T_VkSurfaceFormatKHR = record
          format      :T_VkFormat;
          colorSpace  :T_VkColorSpaceKHR;
     end;

type PFN_vkDestroySurfaceKHR                       = procedure( instance_:T_VkInstance; surface_:T_VkSurfaceKHR; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetPhysicalDeviceSurfaceSupportKHR      = function( physicalDevice_:T_VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; surface_:T_VkSurfaceKHR; pSupported_:P_VkBool32 ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR = function( physicalDevice_:T_VkPhysicalDevice; surface_:T_VkSurfaceKHR; pSurfaceCapabilities_:P_VkSurfaceCapabilitiesKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceSurfaceFormatsKHR      = function( physicalDevice_:T_VkPhysicalDevice; surface_:T_VkSurfaceKHR; pSurfaceFormatCount_:P_uint32_t; pSurfaceFormats_:P_VkSurfaceFormatKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceSurfacePresentModesKHR = function( physicalDevice_:T_VkPhysicalDevice; surface_:T_VkSurfaceKHR; pPresentModeCount_:P_uint32_t; pPresentModes_:P_VkPresentModeKHR ) :T_VkResult; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
procedure vkDestroySurfaceKHR(
          instance_  :T_VkInstance;
          surface_   :T_VkSurfaceKHR;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceSupportKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          queueFamilyIndex_ :T_uint32_t;
          surface_          :T_VkSurfaceKHR;
         pSupported_        :P_VkBool32         ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceCapabilitiesKHR(
          physicalDevice_      :T_VkPhysicalDevice;
          surface_             :T_VkSurfaceKHR;
         pSurfaceCapabilities_ :P_VkSurfaceCapabilitiesKHR ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfaceFormatsKHR(
          physicalDevice_     :T_VkPhysicalDevice;
          surface_            :T_VkSurfaceKHR;
         pSurfaceFormatCount_ :P_uint32_t;
         pSurfaceFormats_     :P_VkSurfaceFormatKHR ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceSurfacePresentModesKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          surface_          :T_VkSurfaceKHR;
         pPresentModeCount_ :P_uint32_t;
         pPresentModes_     :P_VkPresentModeKHR ) :T_VkResult; stdcall; external DLLNAME;
{$ENDIF}

const VK_KHR_swapchain = 1;
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or Defined( __x86_64__ ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
type T_VkSwapchainKHR = ^VkSwapchainKHR_T; VkSwapchainKHR_T = record end;
{$ELSE}
type T_VkSwapchainKHR = T_uint64_t;
{$ENDIF}
type P_VkSwapchainKHR = ^T_VkSwapchainKHR;

const VK_KHR_SWAPCHAIN_SPEC_VERSION   = 67;
const VK_KHR_SWAPCHAIN_EXTENSION_NAME = 'VK_KHR_swapchain';

type T_VkSwapchainCreateFlagsKHR = T_VkFlags;

type P_VkSwapchainCreateInfoKHR = ^T_VkSwapchainCreateInfoKHR;
     T_VkSwapchainCreateInfoKHR = record
         sType                  :T_VkStructureType;
         pNext                  :P_void;
          flags                 :T_VkSwapchainCreateFlagsKHR;
          surface               :T_VkSurfaceKHR;
          minImageCount         :T_uint32_t;
          imageFormat           :T_VkFormat;
          imageColorSpace       :T_VkColorSpaceKHR;
          imageExtent           :T_VkExtent2D;
          imageArrayLayers      :T_uint32_t;
          imageUsage            :T_VkImageUsageFlags;
          imageSharingMode      :T_VkSharingMode;
          queueFamilyIndexCount :T_uint32_t;
         pQueueFamilyIndices    :P_uint32_t;
          preTransform          :T_VkSurfaceTransformFlagBitsKHR;
          compositeAlpha        :T_VkCompositeAlphaFlagBitsKHR;
          presentMode           :T_VkPresentModeKHR;
          clipped               :T_VkBool32;
          oldSwapchain          :T_VkSwapchainKHR;
     end;

type P_VkPresentInfoKHR = ^T_VkPresentInfoKHR;
     T_VkPresentInfoKHR = record
         sType               :T_VkStructureType;
         pNext               :P_void;
          waitSemaphoreCount :T_uint32_t;
         pWaitSemaphores     :P_VkSemaphore;
          swapchainCount     :T_uint32_t;
         pSwapchains         :P_VkSwapchainKHR;
         pImageIndices       :P_uint32_t;
         pResults            :P_VkResult;
     end;

type PFN_vkCreateSwapchainKHR    = function( device_:T_VkDevice; const pCreateInfo_:P_VkSwapchainCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSwapchain_:P_VkSwapchainKHR ) :T_VkResult; stdcall;
type PFN_vkDestroySwapchainKHR   = procedure( device_:T_VkDevice; swapchain_:T_VkSwapchainKHR; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkGetSwapchainImagesKHR = function( device_:T_VkDevice; swapchain_:T_VkSwapchainKHR; pSwapchainImageCount_:P_uint32_t; pSwapchainImages_:P_VkImage ) :T_VkResult; stdcall;
type PFN_vkAcquireNextImageKHR   = function( device_:T_VkDevice; swapchain_:T_VkSwapchainKHR; timeout_:T_uint64_t; semaphore_:T_VkSemaphore; fence_:T_VkFence; pImageIndex_:P_uint32_t ) :T_VkResult; stdcall;
type PFN_vkQueuePresentKHR       = function( queue_:T_VkQueue; const pPresentInfo_:P_VkPresentInfoKHR ) :T_VkResult; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateSwapchainKHR(
          device_     :T_VkDevice;
  const  pCreateInfo_ :P_VkSwapchainCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSwapchain_  :P_VkSwapchainKHR           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroySwapchainKHR(
          device_    :T_VkDevice;
          swapchain_ :T_VkSwapchainKHR;
  const  pAllocator_ :P_VkAllocationCallbacks ); stdcall; external DLLNAME;

function vkGetSwapchainImagesKHR(
          device_              :T_VkDevice;
          swapchain_           :T_VkSwapchainKHR;
         pSwapchainImageCount_ :P_uint32_t;
         pSwapchainImages_     :P_VkImage        ) :T_VkResult; stdcall; external DLLNAME;

function vkAcquireNextImageKHR(
          device_     :T_VkDevice;
          swapchain_  :T_VkSwapchainKHR;
          timeout_    :T_uint64_t;
          semaphore_  :T_VkSemaphore;
          fence_      :T_VkFence;
         pImageIndex_ :P_uint32_t       ) :T_VkResult; stdcall; external DLLNAME;

function vkQueuePresentKHR(
          queue_ :T_VkQueue;
  const  pPresentInfo_:P_VkPresentInfoKHR ) :T_VkResult; stdcall; external DLLNAME;
{$ENDIF}

const VK_KHR_display = 1;
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or Defined( __x86_64__ ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
type T_VkDisplayKHR     = ^VkDisplayKHR_T;     VkDisplayKHR_T     = record end;
type T_VkDisplayModeKHR = ^VkDisplayModeKHR_T; VkDisplayModeKHR_T = record end;
{$ELSE}
type T_VkDisplayKHR     = T_uint64_t;
type T_VkDisplayModeKHR = T_uint64_t;
{$ENDIF}
type P_VkDisplayKHR     = ^T_VkDisplayKHR;
type P_VkDisplayModeKHR = ^T_VkDisplayModeKHR;

const VK_KHR_DISPLAY_SPEC_VERSION   = 21;
const VK_KHR_DISPLAY_EXTENSION_NAME = 'VK_KHR_display';

type P_VkDisplayPlaneAlphaFlagBitsKHR = ^T_VkDisplayPlaneAlphaFlagBitsKHR;
     T_VkDisplayPlaneAlphaFlagBitsKHR = (
       VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR                  = $00000001,
       VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR                  = $00000002,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR               = $00000004,
       VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = $00000008
     );
type T_VkDisplayModeCreateFlagsKHR    = T_VkFlags;
type T_VkDisplayPlaneAlphaFlagsKHR    = T_VkFlags;
type T_VkDisplaySurfaceCreateFlagsKHR = T_VkFlags;

type P_VkDisplayPropertiesKHR = ^T_VkDisplayPropertiesKHR;
     T_VkDisplayPropertiesKHR = record
          display              :T_VkDisplayKHR;
          displayName          :P_char;
          physicalDimensions   :T_VkExtent2D;
          physicalResolution   :T_VkExtent2D;
          supportedTransforms  :T_VkSurfaceTransformFlagsKHR;
          planeReorderPossible :T_VkBool32;
          persistentContent    :T_VkBool32;
     end;

type P_VkDisplayModeParametersKHR = ^T_VkDisplayModeParametersKHR;
     T_VkDisplayModeParametersKHR = record
          visibleRegion  :T_VkExtent2D;
          refreshRate    :T_uint32_t;
     end;

type P_VkDisplayModePropertiesKHR = ^T_VkDisplayModePropertiesKHR;
     T_VkDisplayModePropertiesKHR = record
          displayMode  :T_VkDisplayModeKHR;
          parameters   :T_VkDisplayModeParametersKHR;
     end;

type P_VkDisplayModeCreateInfoKHR = ^T_VkDisplayModeCreateInfoKHR;
     T_VkDisplayModeCreateInfoKHR = record
         sType       :T_VkStructureType;
         pNext       :P_void;
          flags      :T_VkDisplayModeCreateFlagsKHR;
          parameters :T_VkDisplayModeParametersKHR;
     end;

type P_VkDisplayPlaneCapabilitiesKHR = ^T_VkDisplayPlaneCapabilitiesKHR;
     T_VkDisplayPlaneCapabilitiesKHR = record
          supportedAlpha  :T_VkDisplayPlaneAlphaFlagsKHR;
          minSrcPosition  :T_VkOffset2D;
          maxSrcPosition  :T_VkOffset2D;
          minSrcExtent    :T_VkExtent2D;
          maxSrcExtent    :T_VkExtent2D;
          minDstPosition  :T_VkOffset2D;
          maxDstPosition  :T_VkOffset2D;
          minDstExtent    :T_VkExtent2D;
          maxDstExtent    :T_VkExtent2D;
     end;

type P_VkDisplayPlanePropertiesKHR = ^T_VkDisplayPlanePropertiesKHR;
     T_VkDisplayPlanePropertiesKHR = record
          currentDisplay     :T_VkDisplayKHR;
          currentStackIndex  :T_uint32_t;
     end;

type P_VkDisplaySurfaceCreateInfoKHR = ^T_VkDisplaySurfaceCreateInfoKHR;
     T_VkDisplaySurfaceCreateInfoKHR = record
         sType            :T_VkStructureType;
         pNext            :P_void;
          flags           :T_VkDisplaySurfaceCreateFlagsKHR;
          displayMode     :T_VkDisplayModeKHR;
          planeIndex      :T_uint32_t;
          planeStackIndex :T_uint32_t;
          transform       :T_VkSurfaceTransformFlagBitsKHR;
          globalAlpha     :T_float;
          alphaMode       :T_VkDisplayPlaneAlphaFlagBitsKHR;
          imageExtent     :T_VkExtent2D;
     end;

type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR      = function( physicalDevice_:T_VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayPropertiesKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = function( physicalDevice_:T_VkPhysicalDevice; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayPlanePropertiesKHR ) :T_VkResult; stdcall;
type PFN_vkGetDisplayPlaneSupportedDisplaysKHR        = function( physicalDevice_:T_VkPhysicalDevice; planeIndex_:T_uint32_t; pDisplayCount_:P_uint32_t; pDisplays_:P_VkDisplayKHR ) :T_VkResult; stdcall;
type PFN_vkGetDisplayModePropertiesKHR                = function( physicalDevice_:T_VkPhysicalDevice; display_:T_VkDisplayKHR; pPropertyCount_:P_uint32_t; pProperties_:P_VkDisplayModePropertiesKHR ) :T_VkResult; stdcall;
type PFN_vkCreateDisplayModeKHR                       = function( physicalDevice_:T_VkPhysicalDevice; display_:T_VkDisplayKHR; const pCreateInfo_:P_VkDisplayModeCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pMode_:P_VkDisplayModeKHR ) :T_VkResult; stdcall;
type PFN_vkGetDisplayPlaneCapabilitiesKHR             = function( physicalDevice_:T_VkPhysicalDevice; mode_:T_VkDisplayModeKHR; planeIndex_:T_uint32_t; pCapabilities_:P_VkDisplayPlaneCapabilitiesKHR ) :T_VkResult; stdcall;
type PFN_vkCreateDisplayPlaneSurfaceKHR               = function( instance_:T_VkInstance; const pCreateInfo_:P_VkDisplaySurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceDisplayPropertiesKHR(
          physicalDevice_:T_VkPhysicalDevice;
         pPropertyCount_ :P_uint32_t;
         pProperties_    :P_VkDisplayPropertiesKHR ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceDisplayPlanePropertiesKHR(
          physicalDevice_:T_VkPhysicalDevice;
         pPropertyCount_ :P_uint32_t;
         pProperties_    :P_VkDisplayPlanePropertiesKHR ) :T_VkResult; stdcall; external DLLNAME;

function vkGetDisplayPlaneSupportedDisplaysKHR(
         physicalDevice_ :T_VkPhysicalDevice;
         planeIndex_      :T_uint32_t;
        pDisplayCount_   :P_uint32_t;
        pDisplays_       :P_VkDisplayKHR     ) :T_VkResult; stdcall; external DLLNAME;

function vkGetDisplayModePropertiesKHR(
         physicalDevice_ :T_VkPhysicalDevice;
         display_        :T_VkDisplayKHR;
        pPropertyCount_  :P_uint32_t;
        pProperties_     :P_VkDisplayModePropertiesKHR ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateDisplayModeKHR(
          physicalDevice_ :T_VkPhysicalDevice;
          display_        :T_VkDisplayKHR;
  const  pCreateInfo_     :P_VkDisplayModeCreateInfoKHR;
  const  pAllocator_      :P_VkAllocationCallbacks;
         pMode_           :P_VkDisplayModeKHR           ) :T_VkResult; stdcall; external DLLNAME;

function vkGetDisplayPlaneCapabilitiesKHR(
          physicalDevice_ :T_VkPhysicalDevice;
          mode_           :T_VkDisplayModeKHR;
          planeIndex_     :T_uint32_t;
         pCapabilities_   :P_VkDisplayPlaneCapabilitiesKHR ) :T_VkResult; stdcall; external DLLNAME;

function vkCreateDisplayPlaneSurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkDisplaySurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR                  ) :T_VkResult; stdcall; external DLLNAME;
{$ENDIF}

const VK_KHR_display_swapchain                = 1;
const VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION   = 9;
const VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = 'VK_KHR_display_swapchain';

type P_VkDisplayPresentInfoKHR = ^T_VkDisplayPresentInfoKHR;
     T_VkDisplayPresentInfoKHR = record
         sType       :T_VkStructureType;
         pNext       :P_void;
          srcRect    :T_VkRect2D;
          dstRect    :T_VkRect2D;
          persistent :T_VkBool32;
     end;

type PFN_vkCreateSharedSwapchainsKHR = function( device_:T_VkDevice; swapchainCount_:T_uint32_t; const pCreateInfos_:P_VkSwapchainCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSwapchains_:P_VkSwapchainKHR ) :T_VkResult; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateSharedSwapchainsKHR(
          device_         :T_VkDevice;
          swapchainCount_ :T_uint32_t;
  const  pCreateInfos_    :P_VkSwapchainCreateInfoKHR;
  const  pAllocator_      :P_VkAllocationCallbacks;
         pSwapchains_     :P_VkSwapchainKHR           ) :T_VkResult; stdcall; external DLLNAME;
{$ENDIF}

{$IFDEF VK_USE_PLATFORM_XLIB_KHR }
const VK_KHR_xlib_surface = 1;
//#include <X11/Xlib.h>

const VK_KHR_XLIB_SURFACE_SPEC_VERSION   = 6;
const VK_KHR_XLIB_SURFACE_EXTENSION_NAME = 'VK_KHR_xlib_surface';

type T_VkXlibSurfaceCreateFlagsKHR = T_VkFlags;

type P_VkXlibSurfaceCreateInfoKHR = ^T_VkXlibSurfaceCreateInfoKHR;
     T_VkXlibSurfaceCreateInfoKHR = record
         sType   :T_VkStructureType;
         pNext   :P_void;
          flags  :T_VkXlibSurfaceCreateFlagsKHR;
          dpy    :P_Display;
          window :T_Window;
     end;

type PFN_vkCreateXlibSurfaceKHR                        = function( instance_:T_VkInstance; const pCreateInfo_:P_VkXlibSurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR = function( physicalDevice_:T_VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; dpy_:P_Display; visualID_:T_VisualID ) :T_VkBool32; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateXlibSurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkXlibSurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR               ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceXlibPresentationSupportKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          queueFamilyIndex_ :T_uint32_t;
          dpy_              :P_Display;
          visualID_         :T_VisualID         ) :T_VkBool32; stdcall; external DLLNAME;
{$ENDIF}
{$ENDIF} (* VK_USE_PLATFORM_XLIB_KHR *)

{$IFDEF VK_USE_PLATFORM_XCB_KHR }
const VK_KHR_xcb_surface = 1;
//#include <xcb/xcb.h>

const VK_KHR_XCB_SURFACE_SPEC_VERSION   = 6;
const VK_KHR_XCB_SURFACE_EXTENSION_NAME = 'VK_KHR_xcb_surface';

type T_VkXcbSurfaceCreateFlagsKHR = T_VkFlags;

type P_VkXcbSurfaceCreateInfoKHR = ^T_VkXcbSurfaceCreateInfoKHR;
     T_VkXcbSurfaceCreateInfoKHR = record
         sType       :T_VkStructureType;
         pNext       :P_void;
          flags      :T_VkXcbSurfaceCreateFlagsKHR;
          connection :P_xcb_connection_t;
          window     :T_xcb_window_t;
     end;

type PFN_vkCreateXcbSurfaceKHR                        = function( instance_:T_VkInstance; const pCreateInfo_:P_VkXcbSurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR = function( physicalDevice_:T_VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; connection_:P_xcb_connection_t; visual_id_:T_xcb_visualid_t ) :T_VkBool32; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateXcbSurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkXcbSurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR              ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceXcbPresentationSupportKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          queueFamilyIndex_ :T_uint32_t;
          connection_       :P_xcb_connection_t;
          visual_id_        :T_xcb_visualid_t   ) :T_VkBool32; stdcall; external DLLNAME;
{$ENDIF}
{$ENDIF} (* VK_USE_PLATFORM_XCB_KHR *)

{$IFDEF VK_USE_PLATFORM_WAYLAND_KHR }
const VK_KHR_wayland_surface = 1;
//#include <wayland-client.h>

const VK_KHR_WAYLAND_SURFACE_SPEC_VERSION   = 5;
const VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME = 'VK_KHR_wayland_surface';

type T_VkWaylandSurfaceCreateFlagsKHR = T_VkFlags;

type P_VkWaylandSurfaceCreateInfoKHR = ^T_VkWaylandSurfaceCreateInfoKHR;
     T_VkWaylandSurfaceCreateInfoKHR = record
         sType    :T_VkStructureType;
         pNext    :P_void;
          flags   :T_VkWaylandSurfaceCreateFlagsKHR;
          display :P_wl_display;
          surface :P_wl_surface;
     end;

type PFN_vkCreateWaylandSurfaceKHR                        = function( instance_:T_VkInstance; const pCreateInfo_:P_VkWaylandSurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR = function( physicalDevice_:T_VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; display_:P_wl_display ) :T_VkBool32; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateWaylandSurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkWaylandSurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR                  ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceWaylandPresentationSupportKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          queueFamilyIndex_ :T_uint32_t;
          display_          :P_wl_display       ) :T_VkBool32; stdcall; external DLLNAME;
{$ENDIF}
{$ENDIF} (* VK_USE_PLATFORM_WAYLAND_KHR *)

{$IFDEF VK_USE_PLATFORM_MIR_KHR }
const VK_KHR_mir_surface = 1;
//#include <mir_toolkit/client_types.h>

const VK_KHR_MIR_SURFACE_SPEC_VERSION   = 4;
const VK_KHR_MIR_SURFACE_EXTENSION_NAME = 'VK_KHR_mir_surface';

type T_VkMirSurfaceCreateFlagsKHR = T_VkFlags;

type P_VkMirSurfaceCreateInfoKHR = ^T_VkMirSurfaceCreateInfoKHR;
     T_VkMirSurfaceCreateInfoKHR = record
         sType       :T_VkStructureType;
         pNext       :P_void;
          flags      :T_VkMirSurfaceCreateFlagsKHR;
          connection :P_MirConnection;
          mirSurface :P_MirSurface;
     end;

type PFN_vkCreateMirSurfaceKHR                        = function( instance_:T_VkInstance; const pCreateInfo_:P_VkMirSurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceMirPresentationSupportKHR = function( physicalDevice_:T_VkPhysicalDevice; queueFamilyIndex_:T_uint32_t; connection_:P_MirConnection ) :T_VkBool32; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateMirSurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkMirSurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR              ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceMirPresentationSupportKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          queueFamilyIndex_ :T_uint32_t;
          connection_       :P_MirConnection    ) :T_VkBool32; stdcall; external DLLNAME;
{$ENDIF}
{$ENDIF} (* VK_USE_PLATFORM_MIR_KHR *)

{$IFDEF VK_USE_PLATFORM_ANDROID_KHR }
const VK_KHR_android_surface = 1;
//#include <android/native_window.h>

const VK_KHR_ANDROID_SURFACE_SPEC_VERSION   = 6;
const VK_KHR_ANDROID_SURFACE_EXTENSION_NAME = 'VK_KHR_android_surface';

type T_VkAndroidSurfaceCreateFlagsKHR = T_VkFlags;

type P_VkAndroidSurfaceCreateInfoKHR = ^T_VkAndroidSurfaceCreateInfoKHR;
     T_VkAndroidSurfaceCreateInfoKHR = record
         sType  :T_VkStructureType;
         pNext  :P_void;
         flags  :T_VkAndroidSurfaceCreateFlagsKHR;
         window :P_ANativeWindow;
     end;

type PFN_vkCreateAndroidSurfaceKHR = function( instance_:T_VkInstance; const pCreateInfo_:P_VkAndroidSurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateAndroidSurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkAndroidSurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR                  ) :T_VkResult; stdcall; external DLLNAME;
{$ENDIF}
{$ENDIF} (* VK_USE_PLATFORM_ANDROID_KHR *)

{$IFDEF VK_USE_PLATFORM_WIN32_KHR }
const VK_KHR_win32_surface = 1;
//#include <windows.h>

const VK_KHR_WIN32_SURFACE_SPEC_VERSION   = 5;
const VK_KHR_WIN32_SURFACE_EXTENSION_NAME = 'VK_KHR_win32_surface';

type T_VkWin32SurfaceCreateFlagsKHR = T_VkFlags;

type P_VkWin32SurfaceCreateInfoKHR = ^T_VkWin32SurfaceCreateInfoKHR;
     T_VkWin32SurfaceCreateInfoKHR = record
         sType      :T_VkStructureType;
         pNext      :P_void;
          flags     :T_VkWin32SurfaceCreateFlagsKHR;
          hinstance :T_HINSTANCE;
          hwnd      :T_HWND;
     end;

type PFN_vkCreateWin32SurfaceKHR                        = function( instance_:T_VkInstance; const pCreateInfo_:P_VkWin32SurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :T_VkResult; stdcall;
type PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR = function( physicalDevice_:T_VkPhysicalDevice; queueFamilyIndex_:T_uint32_t ) :T_VkBool32; stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateWin32SurfaceKHR(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkWin32SurfaceCreateInfoKHR;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pSurface_    :P_VkSurfaceKHR                ) :T_VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceWin32PresentationSupportKHR(
          physicalDevice_   :T_VkPhysicalDevice;
          queueFamilyIndex_ :T_uint32_t         ) :T_VkBool32; stdcall; external DLLNAME;
{$ENDIF}
{$ENDIF} (* VK_USE_PLATFORM_WIN32_KHR *)

const VK_EXT_debug_report = 1;
{$IF Defined( __LP64__ ) or Defined( _WIN64 ) or Defined( __x86_64__ ) or Defined( _M_X64 ) or Defined( __ia64 ) or Defined( _M_IA64 ) or Defined( __aarch64__ ) or Defined( __powerpc64__ ) }
type T_VkDebugReportCallbackEXT = ^VkDebugReportCallbackEXT_T; VkDebugReportCallbackEXT_T = record end;
{$ELSE}
type T_VkDebugReportCallbackEXT = T_uint64_t;
{$ENDIF}
type P_VkDebugReportCallbackEXT = ^T_VkDebugReportCallbackEXT;

const VK_EXT_DEBUG_REPORT_SPEC_VERSION   = 1;
const VK_EXT_DEBUG_REPORT_EXTENSION_NAME = 'VK_EXT_debug_report';

type P_VkDebugReportObjectTypeEXT = ^T_VkDebugReportObjectTypeEXT;
     T_VkDebugReportObjectTypeEXT = (
       VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT               =  0,
       VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT              =  1,
       VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT       =  2,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT                =  3,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT                 =  4,
       VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT             =  5,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT        =  6,
       VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT                 =  7,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT         =  8,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT                =  9,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT                 = 10,
       VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT                 = 11,
       VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT            = 12,
       VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT           = 13,
       VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT            = 14,
       VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT         = 15,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT        = 16,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT       = 17,
       VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT           = 18,
       VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT              = 19,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = 20,
       VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT               = 21,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT       = 22,
       VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT        = 23,
       VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT           = 24,
       VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT          = 25,
       VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT           = 26,
       VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT         = 27,
       VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT          = 28
     );

type P_VkDebugReportErrorEXT = ^T_VkDebugReportErrorEXT;
     T_VkDebugReportErrorEXT = (
       VK_DEBUG_REPORT_ERROR_NONE_EXT         = 0,
       VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT = 1
     );

type P_VkDebugReportFlagBitsEXT = ^T_VkDebugReportFlagBitsEXT;
     T_VkDebugReportFlagBitsEXT = (
       VK_DEBUG_REPORT_INFORMATION_BIT_EXT         = $00000001,
       VK_DEBUG_REPORT_WARNING_BIT_EXT             = $00000002,
       VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = $00000004,
       VK_DEBUG_REPORT_ERROR_BIT_EXT               = $00000008,
       VK_DEBUG_REPORT_DEBUG_BIT_EXT               = $00000010
     );
type T_VkDebugReportFlagsEXT = T_VkFlags;

type PFN_vkDebugReportCallbackEXT = function(
          flags_       :T_VkDebugReportFlagsEXT;
          objectType_  :T_VkDebugReportObjectTypeEXT;
          object_      :T_uint64_t;
          location_    :T_size_t;
          messageCode_ :T_int32_t;
  const pLayerPrefix_  :P_char;
  const pMessage_      :P_char;
	      pUserData      :P_void                       ) :T_VkBool32; stdcall;

type P_VkDebugReportCallbackCreateInfoEXT = ^T_VkDebugReportCallbackCreateInfoEXT;
     T_VkDebugReportCallbackCreateInfoEXT = record
         sType     :T_VkStructureType;
         pNext     :P_void;
          flags    :T_VkDebugReportFlagsEXT;
       pfnCallback :PFN_vkDebugReportCallbackEXT;
         pUserData :P_void;
     end;

type PFN_vkCreateDebugReportCallbackEXT  = function( instance_:T_VkInstance; const pCreateInfo_:P_VkDebugReportCallbackCreateInfoEXT; const pAllocator_:P_VkAllocationCallbacks; pCallback_:P_VkDebugReportCallbackEXT ) :T_VkResult; stdcall;
type PFN_vkDestroyDebugReportCallbackEXT = procedure( instance_:T_VkInstance; callback_:T_VkDebugReportCallbackEXT; const pAllocator_:P_VkAllocationCallbacks ); stdcall;
type PFN_vkDebugReportMessageEXT         = procedure( instance_:T_VkInstance; flags_:T_VkDebugReportFlagsEXT; objectType_:T_VkDebugReportObjectTypeEXT; object_:T_uint64_t; location_:T_size_t; messageCode_:T_int32_t; const pLayerPrefix_:P_char; const pMessage_:P_char ); stdcall;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateDebugReportCallbackEXT(
          instance_   :T_VkInstance;
  const  pCreateInfo_ :P_VkDebugReportCallbackCreateInfoEXT;
  const  pAllocator_  :P_VkAllocationCallbacks;
         pCallback_   :P_VkDebugReportCallbackEXT           ) :T_VkResult; stdcall; external DLLNAME;

procedure vkDestroyDebugReportCallbackEXT(
          instance_  :T_VkInstance;
          callback_  :T_VkDebugReportCallbackEXT;
  const  pAllocator_ :P_VkAllocationCallbacks    ); stdcall; external DLLNAME;

procedure vkDebugReportMessageEXT(
          instance_    :T_VkInstance;
          flags_       :T_VkDebugReportFlagsEXT;
          objectType_  :T_VkDebugReportObjectTypeEXT;
          object_      :T_uint64_t;
          location_    :T_size_t;
          messageCode_ :T_int32_t;
  const  pLayerPrefix_ :P_char;
  const  pMessage_     :P_char                    ); stdcall; external DLLNAME;
{$ENDIF}

implementation //############################################################### ■

function VK_MAKE_VERSION( const major_,minor_,patch_:Cardinal ) :Cardinal;
begin
     Result := ( ( major_ ) shl 22 ) or ( ( minor_ ) shl 12 ) or ( patch_ );
end;

function VK_VERSION_MAJOR( version_:Cardinal ) :Cardinal;
begin
     Result := T_uint32_t( version_ ) shr 22;
end;

function VK_VERSION_MINOR( version_:Cardinal ) :Cardinal;
begin
     Result := ( T_uint32_t( version_ ) shr 12 ) and $3ff;
end;

function VK_VERSION_PATCH( version_:Cardinal ) :Cardinal;
begin
     Result := T_uint32_t( version_ ) and $fff;
end;

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

     VK_API_VERSION := VK_MAKE_VERSION( 1, 0, 3 );

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
