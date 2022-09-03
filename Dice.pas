unit Dice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDiceFace = (dfn0, dfn1, dfn2, dfn3, dfn4, dfn5, dfn6);
  TDice = class(TGraphicControl)
  private
    FFaceNumber: TDiceFace;
    FPen: TPen;
    FBrush: TBrush;
    FSel: Boolean;
    procedure SetFace(Value: TDiceFace);
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetSel(Value: Boolean);
    { Private declarations }
  protected
    procedure Paint; override;
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    procedure SetFaceNumber(Sender: TObject);
    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property Height default 100;
    property Width default 100;
    property FaceNumber: TDiceFace read FFaceNumber write SetFace;
    property Selected: Boolean read FSel write SetSel;
    property Align;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDice]);
end;

procedure TDice.SetFace(Value: TDiceFace);
begin
  if FFaceNumber <> Value
    then begin
           FFaceNumber := Value;
           Invalidate;
         end;
end;

constructor TDice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  FPen := TPen.Create;
  FPen.OnChange := SetFaceNumber;
  FBrush := TBrush.Create;
  FBrush.OnChange := SetFaceNumber;
end;

procedure TDice.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TDice.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

destructor TDice.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TDice.SetFaceNumber(Sender: TObject);
begin
  Invalidate;
end;

procedure TDice.Paint;
begin
  Canvas.Pen := FPen;
  Canvas.Brush := FBrush;
//  Height := Width;
  If FSel = False
    Then Begin
           Canvas.Pen := FPen;
           Canvas.Brush := FBrush;
         End
    Else Begin
           Canvas.Pen.Color := FBrush.Color;
           Canvas.Brush.Color := FPen.Color;
         End;
  Canvas.ReFresh;
  Canvas.Rectangle(0, 0, Width, Height);
  If FSel = False
    Then Begin
           Canvas.Pen.Color := FBrush.Color;
           Canvas.Brush.Color := FPen.Color;
         End
    Else Begin
           Canvas.Pen := FPen;
           Canvas.Brush := FBrush;
         End;
  case FFaceNumber of
    dfn1:
      Begin
        Canvas.Ellipse(Width div 2 - ((Width div 5) div 2), Height div 2 + ((Height div 5)div 2), Width div 2 + ((Width div 5) div 2), Height div 2 - ((Height div 5) div 2));
      End;
    dfn2:
      Begin
        Canvas.Ellipse(Width div 5, Height div 5, Width div 5 * 2, Height div 5 * 2);
        Canvas.Ellipse(Width-(Width div 5*2),Height-(Height div 5*2), Width-(Width div 5), Height-(Height div 5));
      End;
    dfn3:
      Begin
        Canvas.Ellipse(Width div 5, Height div 5, Width div 5 * 2, Height div 5 * 2);
        Canvas.Ellipse(Width-(Width div 5*2),Height-(Height div 5*2), Width-(Width div 5), Height-(Height div 5));
        Canvas.Ellipse(Width div 2 - ((Width div 5) div 2), Height div 2 + ((Height div 5)div 2), Width div 2 + ((Width div 5) div 2), Height div 2 - ((Height div 5) div 2));
      End;
    dfn4:
      Begin
        Canvas.Ellipse(Width div 5, Height div 5, Width div 5 * 2, Height div 5 * 2);
        Canvas.Ellipse(Width-(Width div 5*2),Height div 5,Width-(Width div 5),(Height div 5*2));
        Canvas.Ellipse(Width div 5, Height-(Height div 5*2), Width div 5*2, Height-(Height div 5));
        Canvas.Ellipse(Width-(Width div 5*2),Height-(Height div 5*2), Width-(Width div 5), Height-(Height div 5));
      End;
    dfn5:
      begin
        Canvas.Ellipse(Width div 5, Height div 5, Width div 5 * 2, Height div 5 * 2);
        Canvas.Ellipse(Width-(Width div 5*2),Height div 5,Width-(Width div 5),(Height div 5*2));
        Canvas.Ellipse(Width div 5, Height-(Height div 5*2), Width div 5*2, Height-(Height div 5));
        Canvas.Ellipse(Width-(Width div 5*2),Height-(Height div 5*2), Width-(Width div 5), Height-(Height div 5));
        Canvas.Ellipse(Width div 2 - ((Width div 5) div 2), Height div 2 + ((Height div 5)div 2), Width div 2 + ((Width div 5) div 2), Height div 2 - ((Height div 5) div 2));
      end;
    dfn6:
      Begin
        Canvas.Ellipse(Width div 5, Height div 5, Width div 5 * 2, Height div 5 * 2);
        Canvas.Ellipse(Width-(Width div 5*2),Height div 5,Width-(Width div 5),(Height div 5*2));
        Canvas.Ellipse(Width div 5, Height-(Height div 5*2), Width div 5*2, Height-(Height div 5));
        Canvas.Ellipse(Width-(Width div 5*2),Height-(Height div 5*2), Width-(Width div 5), Height-(Height div 5));
        Canvas.Ellipse(Width div 2 - ((Width div 5) div 2), Height div 5, Width div 2 + ((Width div 5) div 2), Height div 5*2);
        Canvas.Ellipse(Width div 2 - ((Width div 5) div 2), Height - (Height div 5*2), Width div 2 + ((Width div 5) div 2), Height - Height div 5);
      End;
  end;
end;

procedure TDice.SetSel(Value: Boolean);
begin
  if FSel <> Value
    then begin
           FSel := Value;
           Invalidate;
         end;
end;

end.
