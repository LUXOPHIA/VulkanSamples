﻿program VulkanSamples;

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
  LUX.GPU.Vulkan.Pipeli in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Pipeli.pas',
  LUX.GPU.Vulkan.Device in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Device.pas',
  vulkan.util_init in '_LIBRARY\vulkan.util_init.pas',
  vulkan.util in '_LIBRARY\vulkan.util.pas',
  LUX.GPU.Vulkan.Instan in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Instan.pas',
  LUX.GPU.Vulkan.Surfac in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Surfac.pas',
  LUX.GPU.Vulkan.Comman in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Comman.pas',
  LUX.GPU.Vulkan.Swapch in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Swapch.pas',
  LUX.GPU.Vulkan.Layere in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Layere.pas',
  LUX.GPU.Vulkan.Pooler in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Pooler.pas',
  LUX.GPU.Vulkan.Framer in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Framer.pas',
  LUX.GPU.Vulkan.Depthr in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Depthr.pas',
  LUX.GPU.Vulkan.Textur in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Textur.pas',
  LUX.GPU.Vulkan.Imager in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Imager.pas',
  LUX.GPU.Vulkan.root in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.root.pas',
  LUX.GPU.Vulkan.Memory in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Memory.pas',
  LUX.GPU.Vulkan.Buffer.Unifor in '_LIBRARY\LUXOPHIA\LUX.GPU.Vulkan\LUX.GPU.Vulkan.Buffer.Unifor.pas',
  LUX.Data.List in '_LIBRARY\LUXOPHIA\LUX\Data\List\LUX.Data.List.pas',
  LUX.Data.List.core in '_LIBRARY\LUXOPHIA\LUX\Data\List\LUX.Data.List.core.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
