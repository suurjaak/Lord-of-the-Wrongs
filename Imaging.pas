(**
 * Image manipulation, reading and caching.
 *
 * @author    Erki Suurjaak
 * @created   02.01.2004
 * @modified  02.11.2011
 *)
unit Imaging;

interface

uses Windows, ElHashList, graphics, dialogs, SysUtils, classes, GR32, DataClasses;

type
  TResizerThreadCallback = procedure(ID: Integer; Resized: TBitmap) of object;

  TResizeType = (rsThumbnail, rsContent);

  TResizeInfo = class(TObject)
  public
    Item: TDataClass;
    ResizeType: TResizeType;
    constructor Create(Item: TDataClass; ResizeType: TResizeType);
  end;

  // Thread object that resizes a picture in the background, leaving GUI
  // responsive. Executes immediately on creation, callback is called with
  // the resulting resized image.
  TResizerThread = class(TThread)
  private
    ID: Integer;
    Original: TBitmap32;
    Resized: TBitmap;
    Callback: TResizerThreadCallback;
    ResizedWidth: Integer;
    ResizedHeight: Integer;
    // Sets the finished resized picture into the data object.
    procedure OnCompletion();
  protected
    procedure Execute(); override;
  public
    constructor Create(ID: Integer; Source: TBitmap; Width, Height: Integer; Callback: TResizerThreadCallback);
    destructor Destroy(); override;
  end;


  TImaging = class(TObject)
  private
    Cache: TElHashList;
  public
    constructor Create();
    destructor Destroy(); override;
    // Returns an image resource by the specified name. Image resources
    // are kept in images.res and embedded into the application on compilation.
    // Optional FileType argument can specify the resource type, e.g. 'png'.
    function GetResource(Name: String; FileType: String = ''): TPicture;
    // Loads the picture from the local disk.
    function LoadPictureFromDisk(FileName: String): TPicture;
    // Loads the picture from the opened stream (bmp/jpg/png/gif).
    function LoadPictureFromStream(Stream: TStream; FileName: String): TPicture;
    // Loads the picture at the specified URL and returns it.
    // The URL should be URL-encoded.
    function LoadPictureFromURL(URL: String): TPicture;
    // Clears the cache
    procedure Clear();
  end;



implementation

uses jpeg, Globals, HttpProt, pngimage, GR32_Resamplers, axctrls, main, ExtCtrls;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..4096 - 1] of TRGBTriple;

constructor TResizeInfo.Create(Item: TDataClass; ResizeType: TResizeType);
begin
  Self.Item := Item;
  Self.ResizeType := ResizeType;
end;


constructor TResizerThread.Create(ID: Integer; Source: TBitmap; Width, Height: Integer; Callback: TResizerThreadCallback);
begin
  inherited Create(False);
  Self.FreeOnTerminate := True;
  Self.ID := ID;
  Self.Original := TBitmap32.Create();
  Self.Original.Assign(Source);
  Self.ResizedWidth := Width;
  Self.ResizedHeight := Height;
  Self.Callback := Callback;
end;


destructor TResizerThread.Destroy();
var I: Integer;
begin
  Original.Free();
  Resized.Free();
end;


procedure TResizerThread.Execute();
var
  Resampler: TDraftResampler;
  Resized32: TBitmap32;
begin
  Resized32 := TBitmap32.Create();
  Resized32.SetSize(ResizedWidth, ResizedHeight);
  Resampler := TDraftResampler.Create();
  Original.Resampler := Resampler;
  Resized32.Draw(Bounds(0, 0, ResizedWidth, ResizedHeight),
                 Bounds(0, 0, Original.Width, Original.Height),
                 Original);
  Resized := TBitmap.Create();
  Resized.Assign(Resized32);
  Resized32.Free();
  Synchronize(OnCompletion);
end;


procedure TResizerThread.OnCompletion();
begin
  Callback(ID, Resized);
end;


constructor TImaging.Create();
begin
  Cache := TElHashList.Create();
end;


function TImaging.GetResource(Name: String; FileType: String = ''): TPicture;
var
  CacheID: String;
  RStream: TResourceStream;
begin
  CacheID := 'resource://' + Name;
  Result := Cache.Item[CacheID];
  if (Result = nil) then begin
    RStream := TResourceStream.Create(hInstance, Name, RT_RCDATA);
    if (ExtractFileExt(Name) = '') and (FileType <> '') then
      Name := Name + '.' + FileType;
    Result := LoadPictureFromStream(RStream, Name);
    RStream.Free();
    Cache.AddItem(CacheID, Result);
  end;
end;


function TImaging.LoadPictureFromStream(Stream: TStream; FileName: String): TPicture;
var
  Graphic: TGraphic;
  FileExtension: String;
  HTTP: THTTPCli;
  PNG: TPNGObject;
  BMP: TBitmap;
  BMP32: TBitmap32;
  TransparentColor: TColor32;
  PixelPtr: PColor32;
  X, Y: Integer;
  Image: TImage;
  AlphaPtr: PByte;
begin
  Result := nil;
  Graphic := nil;
  Stream.Position := 0;
  FileExtension := LowerCase(ExtractFileExt(FileName));
  if (FileExtension = '.bmp') then begin
    Graphic := TBitmap.Create();
    Graphic.LoadFromStream(Stream);
  end else if (FileExtension = '.gif') then begin
    Graphic := TOleGraphic.Create();
    Graphic.LoadFromStream(Stream);
    Image := TImage.Create(nil);
    Image.Picture.Assign(Graphic);

    BMP := TBitmap.Create();
    BMP.Width := Image.Picture.Width;
    BMP.Height := Image.Picture.Height;
    BMP.Canvas.Draw(0, 0, Image.Picture.Graphic);
    Image.Free();
    Graphic.Free();
    Graphic := BMP;
  end else if ((FileExtension = '.jpg') or (FileExtension = '.jpeg')) then begin
    Graphic := TJPEGImage.Create();
    Graphic.LoadFromStream(Stream);
    TJPEGImage(Graphic).DIBNeeded();
  end else if (FileExtension = '.png') then begin
    // Ridiculous workaround: TPNGObject cannot handle 8-bit PNGs with
    // an optimized palette (pictures are forced into standard/web-safe
    // palette. TBitmap32 handles 8-bit PNGs fine, but it does not seem to
    // support transparency, and exposes no information about palette
    // transparency either. So: we load the picture into TPNGObject,
    // copy it over to TBitmap32, get transparency information from TPNGObject,
    // and go over all the image pixels, tweaking the value of those that
    // are of palette transparency. Totally ridiculous, but can't find better
    // packages.
    Graphic := TBitmap.Create();
    PNG := TPNGObject.Create();
    PNG.LoadFromStream(Stream);
    BMP32 := TBitmap32.Create();
    BMP32.Assign(PNG);
    if PNG.TransparencyMode in [ptmBit, ptmPartial] then
      begin
      BMP32.ResetAlpha();
      PixelPtr := PColor32(@BMP32.Bits[0]);
      
      case PNG.TransparencyMode of
        ptmPartial: begin
          if (PNG.Header.ColorType = COLOR_GRAYSCALEALPHA) or
             (PNG.Header.ColorType = COLOR_RGBALPHA) then begin
            for Y := 0 to BMP32.Height - 1 do begin
              AlphaPtr := PByte(PNG.AlphaScanline[Y]);
              for X := 0 to BMP32.Width - 1 do
              begin
                PixelPtr^ := (PixelPtr^ and $00FFFFFF) or (TColor32(AlphaPtr^) shl 24);
                Inc(PixelPtr);
                Inc(AlphaPtr);
              end;
            end;
          end;
        end;
        ptmBit: begin
          TransparentColor := Color32(PNG.TransparentColor);
          for X := 0 to BMP32.Height * BMP32.Width - 1 do
            begin
            if PixelPtr^ = TransparentColor then
              PixelPtr^ := PixelPtr^ and $00FFFFFF;
            Inc(PixelPtr);
            end;
        end;
      end;
      BMP32.DrawMode := dmBlend;


      { Did not come into use, but retaining just in case.
      var
        Clr, NewClr: TColor;
        RGBArray: PRGBTripleArray;
      // Ordinary TBitmap just cannot handle 8-bit data from pngimage,
      // result has been leached of colours (looks like it creates a new
      // palette of standard Windows/web colours.

      BMP := TBitmap.Create();
      BMP.Assign(PNG);
      NewClr := PNG.TransparentColor and $00FFFFFF;
      for Y := 0 to BMP.Height - 1 do
      begin
        RGBArray := BMP.ScanLine[Y];
        for X := 0 to BMP.Width - 1 do
        begin
            with RGBArray[X] do
            begin
              Clr := RGB(rgbtRed, rgbtGreen, rgbtBlue);
              if Clr = PNG.TransparentColor then
                // And this is wrong, this is not the same result as
                // PixelPtr^ and $00FFFFFF
                RGBArray[X].rgbtRed := NewClr and $FF;
                RGBArray[X].rgbtGreen := (NewClr and $FF00) shr 8;
                RGBArray[X].rgbtBlue := (NewClr and $FF0000) shr 16;
            end;
        end;
      end;
      }
    end else
      BMP32.DrawMode := dmOpaque;

    Graphic.Assign(BMP32);
    PNG.Free();
    BMP32.Free();
  end else begin
    raise Exception.Create('ERROR: file extension "' + FileExtension + '" is unsupported (@LoadPictureFromStream()).');
  end;

  if Graphic <> nil then begin
    Result := TPicture.Create();
    Result.Bitmap.Assign(Graphic);
  end;
end;


function TImaging.LoadPictureFromURL(URL: String): TPicture;
var FileType: String;
    HTTP: THTTPCli;
    Stream: TMemoryStream;
    ErrorMessage: String;
begin
  try
    Stream := TMemoryStream.Create();
    HTTP := THTTPCli.Create(nil);
    HTTP.URL := URL;
    HTTP.NoCache := True;
    HTTP.RcvdStream := Stream;
    HTTP.Get();
  except
    on EHttpException do begin
      ErrorMessage := Format('ERROR %d while retrieving "%s": %s', [HTTP.StatusCode, URL, HTTP.ReasonPhrase]);
      Stream.Free();
      HTTP.Free();
      raise Exception.Create(ErrorMessage);
    end;
  end;
  if (HTTP.StatusCode = 200) Then begin
    Result := LoadPictureFromStream(Stream, URL);
  end else begin
    ErrorMessage := Format('ERROR %d while retrieving "%s": %s', [HTTP.StatusCode, URL, HTTP.ReasonPhrase]);
    Stream.Free();
    HTTP.Free();
    raise Exception.Create(ErrorMessage);
  end;
  Stream.Free();
  HTTP.Free();
end;


function TImaging.LoadPictureFromDisk(FileName: String): TPicture;
var FStream: TFileStream;
begin
  Result := nil;
  if (FileExists(FileName)) then begin
    FStream := TFileStream.Create(FileName, fmOpenRead);
    Result := LoadPictureFromStream(FStream, FileName);
    FStream.Free();
  end;
end;


destructor TImaging.Destroy();
begin
  Cache.Clear();
  Cache.Free();
end;


procedure TImaging.Clear();
begin
  Cache.Clear();
end;


end.
