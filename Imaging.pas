(**
 * Image manipulation, reading and caching.
 *
 * @author    Erki Suurjaak
 * @created   02.01.2004
 * @modified  04.11.2011
 *)
unit Imaging;

interface

uses Windows, ElHashList, graphics, dialogs, SysUtils, classes, GR32,
     DataClasses, SyncObjs;

type
  TResizeType = (rsThumbnail, rsContent);

  TResizeCallback = procedure(Resized: TBitmap; ResizeType: TResizeType; Item: TDataClass) of object;

  // Thread object that resizes a picture in the background, leaving GUI
  // responsive. Executes immediately on creation, callback is called with
  // the resulting resized image.
  TResizeThread = class(TThread)
  private
    ID: Integer;
    Original: TBitmap32;
    Resized: TBitmap;
    Width: Integer;
    Height: Integer;
    Item: TDataClass;
    ResizeType: TResizeType;
    Callback: TResizeCallback;
    // Sets the finished resized picture into the data object.
    procedure OnCompletion();
  protected
    procedure Execute(); override;
  public
    constructor Create(ID: Integer; Original: TBitmap; Width, Height: Integer;
                       Item: TDataClass; ResizeType: TResizeType;
                       Callback: TResizeCallback);
    destructor Destroy(); override;
  end;


  TImaging = class(TObject)
  private
    Cache: TElHashList;
    PendingResizeMap: TElHashList;
    ResizeIDCounter: Integer;
    ResizeSection: TCriticalSection;
    // Encodes spaces in the URL
    function EncodeURLSpaces(URL: string): string;
  public
    constructor Create();
    destructor Destroy(); override;
    // Returns an image resource by the specified name. Image resources
    // are kept in images.res and embedded into the application on compilation.
    // Filename argument is used for specifying content type, can
    // be whole filename or just extension, e.g. '.png'|'.bmp'|'.jpg'
    function GetResource(Name: String; Filename: String): TPicture;
    // Loads the picture from the local disk.
    function LoadPictureFromDisk(Filename: String): TPicture;
    // Loads the picture from the opened stream (bmp/jpg/png/gif).
    // Filename argument is used for specifying content type, can
    // be whole filename or just extension, e.g. '.png'|'.bmp'|'.jpg'
    function LoadPictureFromStream(Stream: TStream; Filename: String): TPicture;
    // Loads the picture at the specified URL and returns it.
    // The URL should be URL-encoded.
    function LoadPictureFromURL(URL: String): TPicture;
    // Saves the picture into the specified file. BMP, JPG and PNG
    // formats are supported.
    procedure SavePictureToDisk(Bitmap: TBitmap; Filename: String);
    // Saves the picture into the specified stream,
    // in the format specified in the filename.
    procedure SavePictureToStream(Bitmap: TBitmap; Stream: TStream; Filename: String);
    // Captures a rectangle from the current application window.
    function CaptureRect(Area: TRect): TBitmap;
    // Queues the picture for resizing in a background thread.
    procedure QueueResize(Bitmap: TBitmap; Width, Height: Integer;
                          Item: TDataClass; ResizeType: TResizeType;
                          Callback: TResizeCallback);
  end;



implementation

uses jpeg, Globals, HttpProt, pngimage, GR32_Resamplers, axctrls, main, ExtCtrls;




constructor TResizeThread.Create(ID: Integer; Original: TBitmap; Width, Height: Integer;
                                 Item: TDataClass; ResizeType: TResizeType;
                                 Callback: TResizeCallback);
begin
  inherited Create(False);
  Self.FreeOnTerminate := True;
  Self.ID := ID;
  Self.Original := TBitmap32.Create();
  Self.Original.Assign(Original);
  Self.Width := Width;
  Self.Height := Height;
  Self.Item := Item;
  Self.ResizeType := ResizeType;
  Self.Callback := Callback;
end;


destructor TResizeThread.Destroy();
begin
  Original.Free();
  Resized.Free();
end;


procedure TResizeThread.Execute();
var
  Resampler: TDraftResampler;
  Resized32: TBitmap32;
begin
  Resized32 := TBitmap32.Create();
  Resized32.SetSize(Width, Height);
  Resampler := TDraftResampler.Create(); // For some reason, must not be freed
  Original.Resampler := Resampler;
  Resized32.Draw(Bounds(0, 0, Width, Height),
                 Bounds(0, 0, Original.Width, Original.Height),
                 Original);
  Resized := TBitmap.Create();
  Resized.Assign(Resized32);
  Resized32.Free();
  Synchronize(OnCompletion);
end;


// Sets the finished resized picture into the data object.
procedure TResizeThread.OnCompletion();
begin
   Callback(Resized, ResizeType, Item);
end;


constructor TImaging.Create();
begin
  Cache := TElHashList.Create();
  PendingResizeMap := TElHashList.Create();
  ResizeSection := TCriticalSection.Create();
  ResizeIDCounter := 0;
end;


destructor TImaging.Destroy();
begin
  Cache.Clear();
  Cache.Free();
  PendingResizeMap.Free();
  ResizeSection.Free();
end;


// Returns an image resource by the specified name. Image resources
// are kept in images.res and embedded into the application on compilation.
// Filename argument is used for specifying content type, can
// be whole filename or just extension, e.g. '.png'|'.bmp'|'.jpg'
function TImaging.GetResource(Name: String; Filename: String): TPicture;
var
  CacheID: String;
  RStream: TResourceStream;
begin
  CacheID := 'resource://' + Name;
  Result := Cache.Item[CacheID];
  if (Result = nil) then begin
    RStream := TResourceStream.Create(hInstance, Name, RT_RCDATA);
    Result := LoadPictureFromStream(RStream, Filename);
    RStream.Free();
    Cache.AddItem(CacheID, Result);
  end;
end;


// Loads the picture from the opened stream (bmp/jpg/png/gif).
// Filename argument is used for specifying content type, can
// be whole filename or just extension, e.g. '.png'|'.bmp'|'.jpg'
function TImaging.LoadPictureFromStream(Stream: TStream; Filename: String): TPicture;
var
  Graphic: TGraphic;
  FileExtension: String;
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
      type
        PRGBTripleArray = ^TRGBTripleArray;
        TRGBTripleArray = array[0..4096 - 1] of TRGBTriple;
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


// Loads the picture at the specified URL and returns it.
// The URL should be URL-encoded.
function TImaging.LoadPictureFromURL(URL: String): TPicture;
var HTTP: THTTPCli;
    Stream: TMemoryStream;
    ErrorMessage: String;
begin
  Stream := TMemoryStream.Create();
  HTTP := THTTPCli.Create(nil);
  HTTP.URL := EncodeURLSpaces(URL);
  HTTP.NoCache := True;
  HTTP.RcvdStream := Stream;
  try
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


// Loads the picture from the local disk.
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



// Encodes spaces in the URL
function TImaging.EncodeURLSpaces(URL: string): string;
begin
  Result := StringReplace(URL, ' ', '%20', [rfReplaceAll]);
end;


// Captures a rectangle from the current application window.
function TImaging.CaptureRect(Area: TRect): TBitmap;
var
  hdcSrc : THandle;
begin
  hdcSrc := GetWindowDC(GetForeGroundWindow);
  try
    Result := TBitmap.Create();
    Result.Width  := Area.Right - Area.Left;
    Result.Height := Area.Bottom - Area.Top;
    StretchBlt(Result.Canvas.Handle, 0, 0, Result.Width,
               Result.Height, hdcSrc, Area.Left, Area.Top,
               Result.Width, Result.Height, SRCCOPY);
  finally
    ReleaseDC(0, hdcSrc);
  end;
end;


// Saves the picture into the specified stream,
// in the format specified in the filename. BMP, JPG and PNG
// formats are supported.
procedure TImaging.SavePictureToStream(Bitmap: TBitmap; Stream: TStream; Filename: String);
var
  JPG: TJPEGImage;
  PNG: TPNGObject;
  FileExtension: String;
begin
  JPG := nil;
  PNG := nil;
  FileExtension := LowerCase(ExtractFileExt(Filename));

  try
    if ('.jpg' = FileExtension) or ('.jpeg' = FileExtension) then begin
      JPG := TJPEGImage.Create();
      JPG.Assign(Bitmap);
      JPG.SaveToStream(Stream);
    end else if '.png' = FileExtension then begin
      PNG := TPNGObject.Create();
      PNG.Assign(Bitmap);
      PNG.SaveToStream(Stream);
    end else begin
      Bitmap.SaveToStream(Stream);
    end;
  finally
    JPG.Free();
    PNG.Free();
  end;
end;

// Saves the picture into the specified file. BMP, JPG and PNG
// formats are supported.
procedure TImaging.SavePictureToDisk(Bitmap: TBitmap; Filename: String);
var
  FileStream: TFileStream;
begin
  FileStream := nil;
  try
    FileStream := TFileStream.Create(Filename, fmCreate);
    SavePictureToStream(Bitmap, FileStream, Filename);
  finally
    FileStream.Free();
  end;
end;


// Queues the picture for resizing in a background thread.
procedure TImaging.QueueResize(Bitmap: TBitmap; Width, Height: Integer;
                                      Item: TDataClass; ResizeType: TResizeType;
                                      Callback: TResizeCallback);
begin
  ResizeSection.Enter();
  TResizeThread.Create(ResizeIDCounter, Bitmap, Width, Height, Item, ResizeType, Callback);
  Inc(ResizeIDCounter);
  ResizeSection.Leave();
end;

end.
