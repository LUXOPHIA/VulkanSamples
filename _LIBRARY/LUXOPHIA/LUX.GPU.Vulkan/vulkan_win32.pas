unit vulkan_win32;

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

uses LUX.Code.C,
     vulkan_core;

const VK_KHR_win32_surface                = 1;
const VK_KHR_WIN32_SURFACE_SPEC_VERSION   = 6;
const VK_KHR_WIN32_SURFACE_EXTENSION_NAME = 'VK_KHR_win32_surface';
type P_VkWin32SurfaceCreateFlagsKHR = ^VkWin32SurfaceCreateFlagsKHR;
     VkWin32SurfaceCreateFlagsKHR = VkFlags;
type P_VkWin32SurfaceCreateInfoKHR = ^VkWin32SurfaceCreateInfoKHR;
     VkWin32SurfaceCreateInfoKHR = record
       sType     :VkStructureType;
       pNext     :P_void;
       flags     :VkWin32SurfaceCreateFlagsKHR;
       hinstance :T_HINSTANCE;
       hwnd      :T_HWND;
     end;

type PFN_vkCreateWin32SurfaceKHR                        = function( instance_:VkInstance; const pCreateInfo_:P_VkWin32SurfaceCreateInfoKHR; const pAllocator_:P_VkAllocationCallbacks; pSurface_:P_VkSurfaceKHR ) :VkResult;
type PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR = function( physicalDevice_:VkPhysicalDevice; queueFamilyIndex_:T_uint32_t ) :VkBool32;

{$IFNDEF VK_NO_PROTOTYPES }
function vkCreateWin32SurfaceKHR(
    instance_          :VkInstance;
    const pCreateInfo_ :P_VkWin32SurfaceCreateInfoKHR;
    const pAllocator_  :P_VkAllocationCallbacks;
    pSurface_          :P_VkSurfaceKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetPhysicalDeviceWin32PresentationSupportKHR(
    physicalDevice_   :VkPhysicalDevice;
    queueFamilyIndex_ :T_uint32_t ) :VkBool32; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_memory_win32                = 1;
const VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION   = 1;
const VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = 'VK_KHR_external_memory_win32';
type P_VkImportMemoryWin32HandleInfoKHR = ^VkImportMemoryWin32HandleInfoKHR;
     VkImportMemoryWin32HandleInfoKHR = record
       sType      :VkStructureType;
       pNext      :P_void;
       handleType :VkExternalMemoryHandleTypeFlagBits;
       handle     :T_HANDLE;
       name       :T_LPCWSTR;
     end;

type P_VkExportMemoryWin32HandleInfoKHR = ^VkExportMemoryWin32HandleInfoKHR;
     VkExportMemoryWin32HandleInfoKHR = record
       sType       :VkStructureType;
       pNext       :P_void;
       pAttributes :P_SECURITY_ATTRIBUTES;
       dwAccess    :T_DWORD;
       name        :T_LPCWSTR;
     end;

type P_VkMemoryWin32HandlePropertiesKHR = ^VkMemoryWin32HandlePropertiesKHR;
     VkMemoryWin32HandlePropertiesKHR = record
       sType          :VkStructureType;
       pNext          :P_void;
       memoryTypeBits :T_uint32_t;
     end;

type P_VkMemoryGetWin32HandleInfoKHR = ^VkMemoryGetWin32HandleInfoKHR;
     VkMemoryGetWin32HandleInfoKHR = record
       sType      :VkStructureType;
       pNext      :P_void;
       memory     :VkDeviceMemory;
       handleType :VkExternalMemoryHandleTypeFlagBits;
     end;

type PFN_vkGetMemoryWin32HandleKHR           = function( device_:VkDevice; const pGetWin32HandleInfo_:P_VkMemoryGetWin32HandleInfoKHR; pHandle_:P_HANDLE ) :VkResult;
type PFN_vkGetMemoryWin32HandlePropertiesKHR = function( device_:VkDevice; handleType_:VkExternalMemoryHandleTypeFlagBits; handle_:T_HANDLE; pMemoryWin32HandleProperties_:P_VkMemoryWin32HandlePropertiesKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetMemoryWin32HandleKHR(
    device_                    :VkDevice;
    const pGetWin32HandleInfo_ :P_VkMemoryGetWin32HandleInfoKHR;
    pHandle_                   :P_HANDLE ) :VkResult; stdcall; external DLLNAME;

function vkGetMemoryWin32HandlePropertiesKHR(
    device_                       :VkDevice;
    handleType_                   :VkExternalMemoryHandleTypeFlagBits;
    handle_                       :T_HANDLE;
    pMemoryWin32HandleProperties_ :P_VkMemoryWin32HandlePropertiesKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_win32_keyed_mutex                = 1;
const VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION   = 1;
const VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME = 'VK_KHR_win32_keyed_mutex';
type P_VkWin32KeyedMutexAcquireReleaseInfoKHR = ^VkWin32KeyedMutexAcquireReleaseInfoKHR;
     VkWin32KeyedMutexAcquireReleaseInfoKHR = record
       sType            :VkStructureType;
       pNext            :P_void;
       acquireCount     :T_uint32_t;
       pAcquireSyncs    :P_VkDeviceMemory;
       pAcquireKeys     :P_uint64_t;
       pAcquireTimeouts :P_uint32_t;
       releaseCount     :T_uint32_t;
       pReleaseSyncs    :P_VkDeviceMemory;
       pReleaseKeys     :P_uint64_t;
     end;



const VK_KHR_external_semaphore_win32                = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION   = 1;
const VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = 'VK_KHR_external_semaphore_win32';
type P_VkImportSemaphoreWin32HandleInfoKHR = ^VkImportSemaphoreWin32HandleInfoKHR;
     VkImportSemaphoreWin32HandleInfoKHR = record
       sType      :VkStructureType;
       pNext      :P_void;
       semaphore  :VkSemaphore;
       flags      :VkSemaphoreImportFlags;
       handleType :VkExternalSemaphoreHandleTypeFlagBits;
       handle     :T_HANDLE;
       name       :T_LPCWSTR;
     end;

type P_VkExportSemaphoreWin32HandleInfoKHR = ^VkExportSemaphoreWin32HandleInfoKHR;
     VkExportSemaphoreWin32HandleInfoKHR = record
       sType       :VkStructureType;
       pNext       :P_void;
       pAttributes :P_SECURITY_ATTRIBUTES;
       dwAccess    :T_DWORD;
       name        :T_LPCWSTR;
     end;

type P_VkD3D12FenceSubmitInfoKHR = ^VkD3D12FenceSubmitInfoKHR;
     VkD3D12FenceSubmitInfoKHR = record
       sType                      :VkStructureType;
       pNext                      :P_void;
       waitSemaphoreValuesCount   :T_uint32_t;
       pWaitSemaphoreValues       :P_uint64_t;
       signalSemaphoreValuesCount :T_uint32_t;
       pSignalSemaphoreValues     :P_uint64_t;
     end;

type P_VkSemaphoreGetWin32HandleInfoKHR = ^VkSemaphoreGetWin32HandleInfoKHR;
     VkSemaphoreGetWin32HandleInfoKHR = record
       sType      :VkStructureType;
       pNext      :P_void;
       semaphore  :VkSemaphore;
       handleType :VkExternalSemaphoreHandleTypeFlagBits;
     end;

type PFN_vkImportSemaphoreWin32HandleKHR = function( device_:VkDevice; const pImportSemaphoreWin32HandleInfo_:P_VkImportSemaphoreWin32HandleInfoKHR ) :VkResult;
type PFN_vkGetSemaphoreWin32HandleKHR    = function( device_:VkDevice; const pGetWin32HandleInfo_:P_VkSemaphoreGetWin32HandleInfoKHR; pHandle_:P_HANDLE ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkImportSemaphoreWin32HandleKHR(
    device_                                :VkDevice;
    const pImportSemaphoreWin32HandleInfo_ :P_VkImportSemaphoreWin32HandleInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetSemaphoreWin32HandleKHR(
    device_                    :VkDevice;
    const pGetWin32HandleInfo_ :P_VkSemaphoreGetWin32HandleInfoKHR;
    pHandle_                   :P_HANDLE ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_KHR_external_fence_win32                = 1;
const VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION   = 1;
const VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = 'VK_KHR_external_fence_win32';
type P_VkImportFenceWin32HandleInfoKHR = ^VkImportFenceWin32HandleInfoKHR;
     VkImportFenceWin32HandleInfoKHR = record
       sType      :VkStructureType;
       pNext      :P_void;
       fence      :VkFence;
       flags      :VkFenceImportFlags;
       handleType :VkExternalFenceHandleTypeFlagBits;
       handle     :T_HANDLE;
       name       :T_LPCWSTR;
     end;

type P_VkExportFenceWin32HandleInfoKHR = ^VkExportFenceWin32HandleInfoKHR;
     VkExportFenceWin32HandleInfoKHR = record
       sType       :VkStructureType;
       pNext       :P_void;
       pAttributes :P_SECURITY_ATTRIBUTES;
       dwAccess    :T_DWORD;
       name        :T_LPCWSTR;
     end;

type P_VkFenceGetWin32HandleInfoKHR = ^VkFenceGetWin32HandleInfoKHR;
     VkFenceGetWin32HandleInfoKHR = record
       sType      :VkStructureType;
       pNext      :P_void;
       fence      :VkFence;
       handleType :VkExternalFenceHandleTypeFlagBits;
     end;

type PFN_vkImportFenceWin32HandleKHR = function( device_:VkDevice; const pImportFenceWin32HandleInfo_:P_VkImportFenceWin32HandleInfoKHR ) :VkResult;
type PFN_vkGetFenceWin32HandleKHR    = function( device_:VkDevice; const pGetWin32HandleInfo_:P_VkFenceGetWin32HandleInfoKHR; pHandle_:P_HANDLE ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkImportFenceWin32HandleKHR(
    device_                            :VkDevice;
    const pImportFenceWin32HandleInfo_ :P_VkImportFenceWin32HandleInfoKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetFenceWin32HandleKHR(
    device_                    :VkDevice;
    const pGetWin32HandleInfo_ :P_VkFenceGetWin32HandleInfoKHR;
    pHandle_                   :P_HANDLE ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_external_memory_win32                = 1;
const VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION   = 1;
const VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = 'VK_NV_external_memory_win32';
type P_VkImportMemoryWin32HandleInfoNV = ^VkImportMemoryWin32HandleInfoNV;
     VkImportMemoryWin32HandleInfoNV = record
       sType      :VkStructureType;
       pNext      :P_void;
       handleType :VkExternalMemoryHandleTypeFlagsNV;
       handle     :T_HANDLE;
     end;

type P_VkExportMemoryWin32HandleInfoNV = ^VkExportMemoryWin32HandleInfoNV;
     VkExportMemoryWin32HandleInfoNV = record
       sType       :VkStructureType;
       pNext       :P_void;
       pAttributes :P_SECURITY_ATTRIBUTES;
       dwAccess    :T_DWORD;
     end;

type PFN_vkGetMemoryWin32HandleNV = function( device_:VkDevice; memory_:VkDeviceMemory; handleType_:VkExternalMemoryHandleTypeFlagsNV; pHandle_:P_HANDLE ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetMemoryWin32HandleNV(
    device_     :VkDevice;
    memory_     :VkDeviceMemory;
    handleType_ :VkExternalMemoryHandleTypeFlagsNV;
    pHandle_    :P_HANDLE ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}


const VK_NV_win32_keyed_mutex                = 1;
const VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION   = 2;
const VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME = 'VK_NV_win32_keyed_mutex';
type P_VkWin32KeyedMutexAcquireReleaseInfoNV = ^VkWin32KeyedMutexAcquireReleaseInfoNV;
     VkWin32KeyedMutexAcquireReleaseInfoNV = record
       sType                       :VkStructureType;
       pNext                       :P_void;
       acquireCount                :T_uint32_t;
       pAcquireSyncs               :P_VkDeviceMemory;
       pAcquireKeys                :P_uint64_t;
       pAcquireTimeoutMilliseconds :P_uint32_t;
       releaseCount                :T_uint32_t;
       pReleaseSyncs               :P_VkDeviceMemory;
       pReleaseKeys                :P_uint64_t;
     end;



const VK_EXT_full_screen_exclusive                = 1;
const VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION   = 4;
const VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = 'VK_EXT_full_screen_exclusive';

type VkFullScreenExclusiveEXT = (
       VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT                = 0,
       VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT                = 1,
       VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT             = 2,
       VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = 3,
       VK_FULL_SCREEN_EXCLUSIVE_MAX_ENUM_EXT               = $7FFFFFFF
     );
type P_VkSurfaceFullScreenExclusiveInfoEXT = ^VkSurfaceFullScreenExclusiveInfoEXT;
     VkSurfaceFullScreenExclusiveInfoEXT = record
       sType               :VkStructureType;
       pNext               :P_void;
       fullScreenExclusive :VkFullScreenExclusiveEXT;
     end;

type P_VkSurfaceCapabilitiesFullScreenExclusiveEXT = ^VkSurfaceCapabilitiesFullScreenExclusiveEXT;
     VkSurfaceCapabilitiesFullScreenExclusiveEXT = record
       sType                        :VkStructureType;
       pNext                        :P_void;
       fullScreenExclusiveSupported :VkBool32;
     end;

type P_VkSurfaceFullScreenExclusiveWin32InfoEXT = ^VkSurfaceFullScreenExclusiveWin32InfoEXT;
     VkSurfaceFullScreenExclusiveWin32InfoEXT = record
       sType    :VkStructureType;
       pNext    :P_void;
       hmonitor :T_HMONITOR;
     end;

type PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT = function( physicalDevice_:VkPhysicalDevice; const pSurfaceInfo_:P_VkPhysicalDeviceSurfaceInfo2KHR; pPresentModeCount_:P_uint32_t; pPresentModes_:P_VkPresentModeKHR ) :VkResult;
type PFN_vkAcquireFullScreenExclusiveModeEXT        = function( device_:VkDevice; swapchain_:VkSwapchainKHR ) :VkResult;
type PFN_vkReleaseFullScreenExclusiveModeEXT        = function( device_:VkDevice; swapchain_:VkSwapchainKHR ) :VkResult;
type PFN_vkGetDeviceGroupSurfacePresentModes2EXT    = function( device_:VkDevice; const pSurfaceInfo_:P_VkPhysicalDeviceSurfaceInfo2KHR; pModes_:P_VkDeviceGroupPresentModeFlagsKHR ) :VkResult;

{$IFNDEF VK_NO_PROTOTYPES }
function vkGetPhysicalDeviceSurfacePresentModes2EXT(
    physicalDevice_     :VkPhysicalDevice;
    const pSurfaceInfo_ :P_VkPhysicalDeviceSurfaceInfo2KHR;
    pPresentModeCount_  :P_uint32_t;
    pPresentModes_      :P_VkPresentModeKHR ) :VkResult; stdcall; external DLLNAME;

function vkAcquireFullScreenExclusiveModeEXT(
    device_    :VkDevice;
    swapchain_ :VkSwapchainKHR ) :VkResult; stdcall; external DLLNAME;

function vkReleaseFullScreenExclusiveModeEXT(
    device_    :VkDevice;
    swapchain_ :VkSwapchainKHR ) :VkResult; stdcall; external DLLNAME;

function vkGetDeviceGroupSurfacePresentModes2EXT(
    device_             :VkDevice;
    const pSurfaceInfo_ :P_VkPhysicalDeviceSurfaceInfo2KHR;
    pModes_             :P_VkDeviceGroupPresentModeFlagsKHR ) :VkResult; stdcall; external DLLNAME;
{$ENDIF}

implementation //############################################################### ■

end. //######################################################################### ■
