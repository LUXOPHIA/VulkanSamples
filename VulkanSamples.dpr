program VulkanSamples;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX.D4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4x4.pas',
  LUX.D4x4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4x4x4.pas',
  LUX in '_LIBRARY\LUXOPHIA\LUX\LUX.pas',
  LUX.D1 in '_LIBRARY\LUXOPHIA\LUX\LUX.D1.pas',
  LUX.D2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2.pas',
  LUX.D2x2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x2.pas',
  LUX.D2x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x4.pas',
  LUX.D2x4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2x4x4.pas',
  LUX.D3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3.pas',
  LUX.D3x3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x3.pas',
  LUX.D3x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x4.pas',
  LUX.D3x4x4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3x4x4.pas',
  LUX.D4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4.pas',
  LUX.Code.C in '_LIBRARY\LUXOPHIA\LUX\Code\LUX.Code.C.pas',
  vulkan_core in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\vulkan_core.pas',
  vulkan_win32 in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\vulkan_win32.pas',
  cube_data in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\_SAMPLES\cube_data.pas',
  LUX.GPU.Vulkan in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.pas',
  LUX.GPU.Vulkan.Buffer in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Buffer.pas',
  LUX.GPU.Vulkan.Shader in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Shader.pas',
  LUX.GPU.Vulkan.Pipeline in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Pipeline.pas',
  LUX.GPU.Vulkan.Device in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Device.pas',
  vulkan.util_init in '_LIBRARY\vulkan.util_init.pas',
  vulkan.util in '_LIBRARY\vulkan.util.pas',
  LUX.GPU.Vulkan.root in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.root.pas',
  LUX.GPU.Vulkan.Instance in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Instance.pas',
  LUX.GPU.Vulkan.Window in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Window.pas',
  LUX.GPU.Vulkan.Surface in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Surface.pas',
  LUX.GPU.Vulkan.Command in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Command.pas',
  LUX.GPU.Vulkan.Swapchain in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Swapchain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
