{
  Minesweeper

  Created: 2025.03.23
  Author: Richard Reed
          davidreed1000@gmail.com
          richard.reed@cgm.com

  *Based on Microsoft's Minesweeper game from the early 1990s.

  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  I'm using two one-dimensional arrays as the base structures:
  - aGrid stores cell values
  - aShow stores cell visibility

  Each element corresponds to a cell; cell indexing is zero-based and
  starts in the top left corner.

  Minimum size is 9 x 5.
  Maximum size is 50 x 50;

  Cell values:
    0   = empty cell with no adjacent mines
    1-8 = number of adjacent cells with mines
    9   = cell has a mine
    +10 = user toggles a flag on/off for a cell by right-clicking. The value
          will inc/dec by 10 as the flag is toggled on/off.

  To follow the game play in code, start with the Paintbox1MouseDown method.

  *MineRatePct is the number of mines as a % of total cells. 12 is the default.
  *If CellDim changes from 25 mine and flag don't paint properly - not sure why.

}
unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.AppEvnts, System.ImageList, Vcl.ImgList, System.Types;

type

  { TAdjacentActionProc }

  TAdjacentActionProc = procedure(ACell: integer) of object;

  { TAdjacentActionProc }

  TfmMain = class(TForm)
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    Panel2: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label2: TLabel;
    lblMines2: TLabel;
    btnNewGame: TSpeedButton;
    Image1: TImage;
    ae: TApplicationEvents;
    il: TImageList;
    lblTimer2: TLabel;
    lblMines: TLabel;
    lblTimer: TLabel;
    tm: TTimer;
    pnlResult: TPanel;
    Shape1: TShape;
    lblMessage: TLabel;
    Image2: TImage;
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure aeIdle(Sender: TObject; var Done: Boolean);
    procedure btnNewGameClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowCell(ACell: integer; AToggleFlag: boolean = False);
  private
    aGrid: array of integer;
    aShow: array of boolean;
    FGridW: integer;
    FGridH: integer;
    FMinesDisplay: integer;
    FMineClick: integer;
    dtStart: TDateTime;
    bInGame: boolean;
    FBmp: TBitmap;
    FTotalMines: integer;
    FCellsRemaining: integer;
    function GetIndexFromMouseCoord(const Ax,Ay: integer): integer;
    procedure AdjacentAction(c: integer; AProc: TAdjacentActionProc);
    procedure IncAction(aCell: integer);
    procedure ShowAction(aCell: integer);
    procedure DoClickCell(ACell: integer; AToggleFlag: boolean = False);
    procedure ResultMessage(AWinner: boolean);
    // procedure RefreshDisplay;
  public
    procedure NewGame(AWidth,AHeight: integer);
  end;

var
  fmMain: TfmMain;

const
  CellDim = 25;
  MineRatePct = 12;
  CellFontColor: array[0..8] of TColor = (clWhite,clBlue,clGreen,clRed,clFuchsia,clBlack,clBlack,clBlack,clBlack);

implementation

{$R *.dfm}

//--------------------------------------------------------------
// FormCreate
//--------------------------------------------------------------
procedure TfmMain.FormCreate(Sender: TObject);
begin
  FBmp := TBitmap.Create;
  btnNewGame.Click;
end;

//--------------------------------------------------------------
// FormDestroy
//--------------------------------------------------------------
procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBmp);
end;

//--------------------------------------------------------------
// aeIdle
//--------------------------------------------------------------
procedure TfmMain.aeIdle(Sender: TObject; var Done: Boolean);
var
  s: string;
begin
  s := FMinesDisplay.ToString;
  if s <> lblMines.Caption then
    lblMines.Caption := s;
end;

//--------------------------------------------------------------
// tmTimer
//--------------------------------------------------------------
procedure TfmMain.tmTimer(Sender: TObject);
begin
  if bInGame then
    lblTimer.Caption := FormatDateTime('nn:ss',Now-dtStart);
end;

//--------------------------------------------------------------
// SpinEdit1Change
//--------------------------------------------------------------
procedure TfmMain.SpinEdit1Change(Sender: TObject);
begin
  SpinEdit2.Value := SpinEdit1.Value;
end;

//--------------------------------------------------------------
// btnNewGameClick
//--------------------------------------------------------------
procedure TfmMain.btnNewGameClick(Sender: TObject);
begin
  NewGame(SpinEdit1.Value,SpinEdit2.Value);
end;

//--------------------------------------------------------------
// NewGame
//--------------------------------------------------------------
procedure TfmMain.NewGame(AWidth, AHeight: integer);
var
  x,y,z: integer;
  r: TRect;

begin
  AnimateWindow(pnlResult.Handle,300,AW_SLIDE or AW_HOR_NEGATIVE or AW_HIDE);
  btnNewGame.ImageIndex := 0;

  // initialize a new game
  SetLength(aGrid,0);
  SetLength(aShow,0);

  FGridW := AWidth;
  FGridH := AHeight;

  SetLength(aGrid,FGridW * FGridH);
  SetLength(aShow,FGridW * FGridH);

  // +93 height, +26 width sizes the form perfectly.
  SetBounds(Left,Top,(FGridW * CellDim) + 26,(FGridH * CellDim) + 93);

  // assign the mines
  Randomize;
  x := 0;
  FTotalMines := MulDiv(Length(aGrid),MineRatePct,100);
  FMinesDisplay := FTotalMines;
  while x < FTotalMines do begin
    z := Random(Length(aGrid)-1);
    if z > Length(aGrid) - 1 then
      Continue;
    if aGrid[z] <> 9 then begin
      aGrid[z] := 9;
      Inc(x);
      AdjacentAction(z,IncAction);
    end;
  end;

  // paint the game board
  r := Rect(0,0,FGridW * CellDim,FGridH * CellDim);
  FBmp.Width := r.Width;
  FBmp.Height := r.Height;

  with FBmp.Canvas do begin
    Brush.Color := clBtnFace;
    FillRect(r);
    // paint the cells one at a time by offsetting the rect
    r := Rect(0,0,CellDim-1,CellDim-1);
    for y := 0 to FGridH - 1 do begin
      for x := 0 to FGridW - 1 do begin
        DrawEdge(Handle,r,EDGE_RAISED,BF_RECT);
        OffsetRect(r,CellDim,0);
      end;
      OffsetRect(r,-(FGridW * CellDim),CellDim);
    end;
  end;

  SpinEdit1.SelLength := 0;
  SpinEdit2.SelLength := 0;
  HideCaret(SpinEdit1.Handle);
  HideCaret(SpinEdit2.Handle);

  FMineClick := -1;
  FCellsRemaining := Length(aGrid);
  dtStart := Now;
  bInGame := True;
  Caption := 'MineSweeper by David Reed';

  PaintBox1.Invalidate;

end;

//--------------------------------------------------------------
// IncAction
//--------------------------------------------------------------
procedure TfmMain.IncAction(aCell: integer);
begin
  if (aCell <= Length(aGrid) - 1) and (aGrid[aCell] <> 9) then
    Inc(aGrid[aCell]);
end;

//--------------------------------------------------------------
// ShowAction
//--------------------------------------------------------------
procedure TfmMain.ShowAction(aCell: integer);
begin
  if (aCell <= Length(aGrid) - 1) and not aShow[aCell] and (aGrid[aCell] < 9) then
    DoClickCell(aCell);
end;

//--------------------------------------------------------------
// AdjacentAction
//--------------------------------------------------------------
procedure TfmMain.AdjacentAction(c: integer; AProc: TAdjacentActionProc);
var
  m,w,h: integer;
begin
  // use w and h to avoid typing FGridW over and over
  w := FGridW;
  h := FGridH;
  // the mod determines left/right cell existence
  m := (c + 1) mod w;
  // check for left/right cells
  if m <> 1 then
    AProc(c-1);
  if m <> 0 then
    AProc(c+1);
  // check for cells above
  if (c > w) then begin
    AProc(c-w);
    if m <> 1 then
      AProc(c-w-1);
    if m <> 0 then
      AProc(c-w+1);
  end;
  // check for cells below
  if (c <= ((w * h) - w)) then begin
    AProc(c+w);
    if m <> 1 then
      AProc(c+w-1);
    if m <> 0 then
      AProc(c+w+1);
  end;
end;

//--------------------------------------------------------------
// DoClickCell
//--------------------------------------------------------------
procedure TfmMain.DoClickCell(ACell: integer; AToggleFlag: boolean = False);
const
  FLAGVALUE: array[boolean] of integer = (1,-1);
begin
  if AToggleFlag then begin
    if not aShow[ACell] then begin
      Inc(aGrid[ACell],FLAGVALUE[aGrid[ACell] > 9] * 10);
      Inc(FMinesDisplay,FLAGVALUE[aGrid[ACell] > 9]);
      ShowCell(ACell,True);
    end;
  end
  // if the cell is a mine then show all mine cells, it's game over
  else if aGrid[ACell] = 9 then begin
    FMineClick := ACell;
    for var x := 0 to Length(aGrid) - 1 do
      if aGrid[x] = 9 then
        ShowCell(x);
  end
  else if (aGrid[ACell] < 9) and not aShow[ACell] then begin
    ShowCell(ACell);
    if aGrid[ACell] = 0 then
      AdjacentAction(ACell,ShowAction);
  end;
end;

//--------------------------------------------------------------
// GetIndexFromMouseCoord
//--------------------------------------------------------------
function TfmMain.GetIndexFromMouseCoord(const Ax, Ay: integer): integer;
begin
  with Point(Ax,Ay) do
    Result := ((Y div CellDim) * FGridW) + (X div CellDim);
end;

//--------------------------------------------------------------
// ResultMessage
//--------------------------------------------------------------
procedure TfmMain.ResultMessage(AWinner: boolean);
begin
  if AWinner then begin
    lblMessage.Caption := 'WOOHOO! You are a winner!';
    Shape1.Brush.Color := $DDFFDD;
    Shape1.Pen.Color := $00DD00;
  end
  else begin
    lblMessage.Caption := 'BOOM! Better luck next time.';
    Shape1.Brush.Color := $DDDDFF;
    Shape1.Pen.Color := $0000DD;
  end;
  with pnlResult do begin
    Width := 200;
    BoundsRect := CenteredRect(Self.ClientRect,ClientRect);
    Top := MulDiv(Self.Height,2,7);
    AnimateWindow(Handle,300,AW_SLIDE or AW_HOR_NEGATIVE);
  end;
end;

//--------------------------------------------------------------
// PaintBox1MouseDown
//--------------------------------------------------------------
procedure TfmMain.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if bInGame then begin
    DoClickCell(GetIndexFromMouseCoord(X,Y),Button = TMouseButton.mbRight);
    Paintbox1.Invalidate;
    if FMineClick >= 0 then begin
      bInGame := False;
      btnNewGame.ImageIndex := 1;
      ResultMessage(False);
    end
    else if FCellsRemaining = FTotalMines then begin
      bInGame := False;
      ResultMessage(True);
    end;
  end;
end;

//--------------------------------------------------------------
// PaintBox1Paint
//--------------------------------------------------------------
procedure TfmMain.PaintBox1Paint(Sender: TObject);
begin
  Paintbox1.Canvas.Draw(0,0,FBmp);
end;

//--------------------------------------------------------------
// ShowCell
//--------------------------------------------------------------
procedure TfmMain.ShowCell(ACell: integer; AToggleFlag: boolean = False);
var
  x,y: integer;
  r,tr: TRect;
  s: string;
begin

  x := (ACell mod FGridW) * CellDim;
  y := (ACell div FGridW) * CellDim;
  r := Rect(x,y,x + CellDim-1,y + CellDim-1);

  with FBmp.Canvas do begin
    if AToggleFlag then begin
      Brush.Color := clBtnFace;
      // draw or erase the flag
      if aGrid[ACell] > 9 then
        TransparentBlt(Handle,r.Left,r.Top,r.Width,r.Height,Image1.Canvas.Handle,25,0,r.Width,r.Height,ColorToRGB(clFuchsia))
      else begin
        FillRect(r);
        DrawEdge(Handle,r,EDGE_RAISED,BF_RECT);
      end;
    end
    else begin
      Pen.Color := clBtnShadow;
      Brush.Color := clWhite;
      if ACell = FMineClick then
        Brush.Color := clRed;
      Rectangle(r);
      // 9 = mine
      if aGrid[ACell] = 9 then begin
        TransparentBlt(Handle,r.Left,r.Top,r.Width,r.Height,Image1.Canvas.Handle,0,0,r.Width,r.Height,ColorToRGB(clFuchsia));
      end
      else begin
        s := ' ';
        if aGrid[ACell] <> 0 then
          s := aGrid[ACell].ToString;
        with r do
          tr := Rect(left,Top,Right,Top);
        DrawText(Handle,PChar(s),Length(s),tr,DT_CENTER or DT_CALCRECT);
        OffSetRect(tr,r.CenterPoint.X - tr.CenterPoint.X,r.CenterPoint.y - tr.CenterPoint.y);
        Font.Color := CellFontColor[aGrid[ACell]];
        DrawText(Handle,PChar(s),Length(s),tr,DT_CENTER or DT_VCENTER);
      end;
      aShow[ACell] := True;
      Dec(FCellsRemaining);
    end;
  end;
end;

// I used this to render the grid in a TLabel first, to check my calculations

//--------------------------------------------------------------
// RefreshDisplay
//--------------------------------------------------------------//
//procedure TForm1.RefreshDisplay;
//var
//  x,y,z: integer;
//  s: string;
//begin
//  for y := 0 to FGridH - 1 do begin
//    for x := 0 to FGridW - 1 do begin
//      z := aGrid[x + (y * FGridW)];
//      if z = 9 then
//        s := s + 'B '
//      else
//        s := s + z.ToString + ' ';
//    end;
//    s := s + #13#10;
//  end;
//end;

end.
