unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, fmx.castlecontrol, CastleUIControls, CastleVectors,
  CastleScene, CastleViewport, CastleTransform, CastleProjection, CastleGLUtils,
  CastleColors, CastleImages, CastleKeysMouse, CastleControls, X3DNodes;

type
  TCastleSceneHelper = class helper for TCastleScene
    function Normalize: Boolean;
  end;

  TCastleApp = class(TCastleView)
    procedure Render; override; // TCastleUserInterface
    procedure Start; override; // TCastleView
    procedure Stop; override; // TCastleView
    procedure Resize; override; // TCastleUserInterface
  private
    ActiveScene: TCastleScene;
    Camera: TCastleCamera;
    CameraLight: TCastleDirectionalLight;
    Viewport: TCastleViewport;
    VPBackImage: TCastleImageControl;
    function LoadScene(filename: String): TCastleScene;
    procedure LoadViewport;
    function  CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure MakeCard(var Scene: TCastleScene; const Expansion: String; const ImageID: String);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure GLWinClick(Sender: TObject);
  private
    { Private declarations }
    GLWin: TCastleControl;
    GLView: TCastleApp;
    CurrentCard: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  datadir: String = '../../data/';
  cardlist: array of String = [ '0001e77a-7fff-49d2-a55c-42f6fdf6db08',
                                '0edb58bb-8ff3-4e34-b3d1-d83b5bd8c178',
                                '0f25e3db-19ae-4f89-80ec-bf0ad561be39',
                                '0f9a993f-1f2b-4b17-b415-e975b7873e18',
                                '1a55c370-d396-4c73-8ee2-83dc4c124005',
                                '1a7cc43c-6e8c-41d2-a885-24604dfc7e7f',
                                '1a8bb9c7-2c4b-48a1-806e-742addb72b4b',
                                '1a9798d6-34b3-4438-992d-d3616a7c8536' ];

function MaskedImage(ImageFile: String): TCastleImage;

implementation

{$R *.fmx}

uses Math;


procedure TForm1.FormCreate(Sender: TObject);
begin
  CurrentCard := 0;
  GLWin := TCastleControl.Create(Self);
  GLWin.OnClick := GLWinClick;
  GLWin.Parent := Form1;
  GLWin.Align := TAlignLayout.Client;
  GLView := TCastleApp.Create(GLWin);
  GLWin.Container.View := GLView;
end;

procedure TForm1.GLWinClick(Sender: TObject);
begin
  with GLView do
    begin
      MakeCard(ActiveScene, 'WOE', cardlist[CurrentCard]);
    end;
  Inc(CurrentCard);
  CurrentCard := CurrentCard mod Length(cardlist);
end;

constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TCastleApp.Render;
begin
  inherited;
end;

function TCastleApp.LoadScene(filename: String): TCastleScene;
begin
  Result := Nil;
  try
    Result := TCastleScene.Create(Self);
    Result.Load(filename);
    Result.Normalize;
  except
    on E : Exception do
      begin
        ShowMessage('Oops #1' + E.ClassName + ' - ' + E.Message);
       end;
  end;
end;

function TCastleApp.CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
var
  Light: TCastleDirectionalLight;
begin
  Light := TCastleDirectionalLight.Create(Self);

  Light.Direction := LightPos;
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;

  Result := Light;
end;

procedure TCastleApp.LoadViewport;
begin
  VPBackImage := TCastleImageControl.Create(Owner);
  VPBackImage.OwnsImage := True;
  VPBackImage.Url := datadir + 'static/wallpaper.png';
  VPBackImage.Stretch := True;
  VPBackImage.Width := Container.Width;
  VPBackImage.Height := Container.Height;

  InsertFront(VPBackImage);

  Viewport := TCastleViewport.Create(Owner);
  Viewport.FullSize := False;
  Viewport.Width := Container.Width;
  Viewport.Height := Container.Height;
  Viewport.Transparent := True;

  Camera := TCastleCamera.Create(Owner);

  Viewport.Setup2D;
  Camera.ProjectionType := ptOrthographic;
  Camera.Orthographic.Width := 1;
  Camera.Orthographic.Height := 1;
  Camera.Orthographic.Origin := Vector2(0.5, 0.5);

  CameraLight := CreateDirectionalLight(Viewport.Camera.Translation);
  Camera.Add(CameraLight);

  Viewport.Items.Add(Camera);
  Viewport.Camera := Camera;

  InsertFront(Viewport);
end;

procedure TCastleApp.Resize;
begin
  Viewport.Width := Container.Width;
  Viewport.Height := Container.Height;
  VPBackImage.Width := Container.Width;
  VPBackImage.Height := Container.Height;
end;

procedure TCastleApp.Start;
begin
  inherited;
  ActiveScene := nil;
  LoadViewport;
  ActiveScene := LoadScene(datadir + 'static/card.x3d');
  if Assigned(ActiveScene) then
    Viewport.Items.Add(ActiveScene);
end;

procedure TCastleApp.Stop;
begin
  inherited;
end;

function TCastleSceneHelper.Normalize: Boolean;
var
  BBMax: Single;
begin
  Result := False;
  if not(RootNode = nil) then
    begin
    if not LocalBoundingBox.IsEmptyOrZero then
      begin
        if LocalBoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + (LocalBoundingBox.SizeX / 2),
                              Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + (LocalBoundingBox.SizeY / 2),
                              Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + (LocalBoundingBox.SizeZ / 2));
            Translation := -Center;

            BBMax := LocalBoundingBox.MaxSize;
            Scale := Vector3(1 / BBMax,
                             1 / BBMax,
                             1 / BBMax);
            Result := True;
          end;
      end;
    end;
end;

function MaskedImage(ImageFile: String): TCastleImage;
var
  rgbaTexture: TRGBAlphaImage;
  rgbImage: TCastleImage;
  mask: TCastleImage;
begin
  if not FileExists(Imagefile) then
    ShowMessage('Missing image : ' + ImageFile);
  rgbImage := LoadImage(ImageFile, [TRGBImage]) as TCastleImage;
  mask := LoadImage(datadir + 'static/frame_alpha.png', [TGrayscaleImage]) as TCastleImage;
  rgbaTexture := TRGBAlphaImage.Create(rgbImage.Width, rgbImage.Height);
  rgbaTexture.Compose(TRGBImage(rgbImage), TGrayscaleImage(mask));
  FreeAndNil(mask);
  FreeAndNil(rgbImage);
  Result := TCastleImage(rgbaTexture);
end;

procedure TCastleApp.MakeCard(var Scene: TCastleScene; const Expansion: String; const ImageID: String);
var
  MyImageTexture: TImageTextureNode;
begin
  MyImageTexture := Scene.Node('Image_Front') as TImageTextureNode;
  MyImageTexture.LoadFromImage(MaskedImage(datadir + 'cards/set_' + Expansion + '/' + ImageID + '.jpg'), True, { ''); // } datadir + 'cards/set_' + Expansion + '/' + ImageID + '.jpg');
end;

end.
