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
  *Realized I don't have a game-won test. I'll write that soon.

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
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure aeIdle(Sender: TObject; var Done: Boolean);
    procedure btnNewGameClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmTimer(Sender: TObject);
  private
    aGrid: array of integer;
    aShow: array of boolean;
    xGridW: integer;
    xGridH: integer;
    xMines: integer;
    xMineClick: integer;
    dtStart: TDateTime;
    bInGame: boolean;
    function GetIndexFromMouseCoord(const Ax,Ay: integer): integer;
    procedure AdjacentAction(c: integer; AProc: TAdjacentActionProc);
    procedure IncAction(aCell: integer);
    procedure ShowAction(aCell: integer);
    procedure DoClickCell(ACell: integer);
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
  btnNewGame.Click;
end;

//--------------------------------------------------------------
// aeIdle
//--------------------------------------------------------------
procedure TfmMain.aeIdle(Sender: TObject; var Done: Boolean);
var
  s: string;
begin
  s := xMines.ToString;
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
  x,z: integer;

begin

  btnNewGame.ImageIndex := 0;

  // initialize a new game
  SetLength(aGrid,0);
  SetLength(aShow,0);

  xGridW := AWidth;
  xGridH := AHeight;

  SetLength(aGrid,xGridW * xGridH);
  SetLength(aShow,xGridW * xGridH);

  // +93 height, +26 width sizes the form perfectly.
  SetBounds(Left,Top,(xGridW * CellDim) + 26,(xGridH * CellDim) + 93);

  // assign the mines
  Randomize;
  x := 0;
  xMines := MulDiv(Length(aGrid),MineRatePct,100);
  while x < xMines do begin
    z := Random(Length(aGrid)-1);
    if z > Length(aGrid) - 1 then
      Continue;
    if aGrid[z] <> 9 then begin
      aGrid[z] := 9;
      Inc(x);
      AdjacentAction(z,IncAction);
    end;
  end;

  SpinEdit1.SelLength := 0;
  SpinEdit2.SelLength := 0;
  HideCaret(SpinEdit1.Handle);
  HideCaret(SpinEdit2.Handle);

  xMineClick := -1;
  dtStart := Now;
  bInGame := True;
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
  if (aCell <= Length(aShow) - 1) and not aShow[aCell] and (aGrid[aCell] < 9) then
    DoClickCell(aCell);
end;

//--------------------------------------------------------------
// AdjacentAction
//--------------------------------------------------------------
procedure TfmMain.AdjacentAction(c: integer; AProc: TAdjacentActionProc);
var
  m,w,h: integer;
begin
  // use w and h to avoid typing xGridW over and over
  w := xGridW;
  h := xGridH;
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
procedure TfmMain.DoClickCell(ACell: integer);
var
  x: integer;
begin
  // if the cell is a mine then show all mine cells, it's game over
  if aGrid[ACell] = 9 then begin
    xMineClick := ACell;
    bInGame := False;
    btnNewGame.ImageIndex := 1;
    for x := 0 to Length(aShow) - 1 do begin
      if aGrid[x] = 9 then
        aShow[x] := True;
    end;
  end
  else begin
    aShow[ACell] := True;
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
    Result := ((Y div CellDim) * xGridW) + (X div CellDim);
end;

//--------------------------------------------------------------
// PaintBox1MouseDown
//--------------------------------------------------------------
procedure TfmMain.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if bInGame then begin
    var z := GetIndexFromMouseCoord(X,Y);
    if Button = TMouseButton.mbLeft then begin
      // >= 10 means the user planted a flag in this cell
      if aGrid[z] < 10 then
        DoClickCell(z);
    end
    else if not aShow[z] then begin
      // this toggles the flag for cells the user believes contain mines
      // adding 10 is a way to preserve the original value of the cell
      if (aGrid[z] < 10) and (xMines > 0) then begin
        Dec(xMines);
        Inc(aGrid[z],10);
      end
      else if (aGrid[z] > 9) then begin
        Inc(xMines);
        Dec(aGrid[z],10);
      end;
    end;
    Paintbox1.Invalidate;
  end;
end;

//--------------------------------------------------------------
// PaintBox1Paint
//--------------------------------------------------------------
procedure TfmMain.PaintBox1Paint(Sender: TObject);
var
  x,y,z: integer;
  r,tr: TRect;
  s: string;
begin
  with PaintBox1.Canvas do begin
    FillRect(Paintbox1.ClientRect);
    // paint the cells one at a time by offsetting the rect
    r := Rect(0,0,CellDim-1,CellDim-1);
    for y := 0 to xGridH - 1 do begin
      for x := 0 to xGridW - 1 do begin
        // compute the cell number
        z := (y * xGridW) + x;
        if aShow[z] then begin
          Pen.Color := clBtnShadow;
          Brush.Color := clWhite;
          if z = xMineClick then
            Brush.Color := clRed;
          Rectangle(r);
          // 9 = mine
          if aGrid[z] = 9 then begin
            TransparentBlt(Handle,r.Left,r.Top,r.Width,r.Height,Image1.Canvas.Handle,0,0,r.Width,r.Height,ColorToRGB(clFuchsia));
          end
          else begin
            s := ' ';
            if aGrid[z] <> 0 then
              s := aGrid[z].ToString;
            with r do
              tr := Rect(left,Top,Right,Top);
            DrawText(Handle,PChar(s),Length(s),tr,DT_CENTER or DT_CALCRECT);
            OffSetRect(tr,r.CenterPoint.X - tr.CenterPoint.X,r.CenterPoint.y - tr.CenterPoint.y);
            Font.Color := CellFontColor[aGrid[z]];
            DrawText(Handle,PChar(s),Length(s),tr,DT_CENTER or DT_VCENTER);
          end;
        end
        else begin
          Brush.Color := clBtnFace;
          DrawEdge(Handle,r,EDGE_RAISED,BF_RECT);
          if aGrid[z] > 9 then
            TransparentBlt(Handle,r.Left,r.Top,r.Width,r.Height,Image1.Canvas.Handle,25,0,r.Width,r.Height,ColorToRGB(clFuchsia));
        end;
        OffsetRect(r,CellDim,0);
      end;
      OffsetRect(r,-(xGridW * CellDim),CellDim);
    end;
  end;
end;

procedure TfmMain.SpinEdit1Change(Sender: TObject);
begin
  SpinEdit2.Value := SpinEdit1.Value;
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
//  for y := 0 to xGridH - 1 do begin
//    for x := 0 to xGridW - 1 do begin
//      z := aGrid[x + (y * xGridW)];
//      if z = 9 then
//        s := s + 'B '
//      else
//        s := s + z.ToString + ' ';
//    end;
//    s := s + #13#10;
//  end;
//end;

end.
