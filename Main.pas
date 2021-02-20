unit Main;

interface //####################################################################

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  vulkan_core,
  vulkan.util, vulkan.util_init;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation //###############################################################

{$R *.fmx}

// https://github.com/LunarG/VulkanSamples/tree/master/API-Samples/01-init_instance

const APP_SHORT_NAME = 'vulkansamples_instance';

procedure TForm1.FormCreate(Sender: TObject);
var
   info      :T_sample_info;
   app_info  :VkApplicationInfo;
   inst_info :VkInstanceCreateInfo;
   inst      :VkInstance;
   res       :VkResult;
begin
     Caption := APP_SHORT_NAME;

     init_global_layer_properties( info );

     (* VULKAN_KEY_START *)

     // initialize the VkApplicationInfo structure
     app_info.sType              := VK_STRUCTURE_TYPE_APPLICATION_INFO;
     app_info.pNext              := nil;
     app_info.pApplicationName   := APP_SHORT_NAME;
     app_info.applicationVersion := 1;
     app_info.pEngineName        := APP_SHORT_NAME;
     app_info.engineVersion      := 1;
     app_info.apiVersion         := VK_API_VERSION_1_0;

     // initialize the VkInstanceCreateInfo structure
     inst_info.sType                   := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
     inst_info.pNext                   := nil;
     inst_info.flags                   := 0;
     inst_info.pApplicationInfo        := @app_info;
     inst_info.enabledExtensionCount   := 0;
     inst_info.ppEnabledExtensionNames := nil;
     inst_info.enabledLayerCount       := 0;
     inst_info.ppEnabledLayerNames     := nil;

     res := vkCreateInstance( @inst_info, nil, @inst );

     Assert( res <> VK_ERROR_INCOMPATIBLE_DRIVER, 'cannot find a compatible Vulkan ICD' );
     Assert( res = VK_SUCCESS, 'unknown error' );

     vkDestroyInstance( inst, nil );

     (* VULKAN_KEY_END *)
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     /////
end;

end. //#########################################################################
