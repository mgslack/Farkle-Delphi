UNIT Center;

INTERFACE

{$IFDEF Win32}
 USES Windows,
{$ELSE}
 USES WinTypes, WinProcs,
{$ENDIF}
      Forms;

{**************************************************************************}

 PROCEDURE CenterPopup(aPopup : TFORM; aParent : HWND);
     (* procedure to center a pop-up window within the parent window *)

{**************************************************************************}

IMPLEMENTATION

{**************************************************************************}

 PROCEDURE CenterPopup(aPopup : TFORM; aParent : HWND);
     (* procedure to center a pop-up window within the parent window *)

    VAR ParentR          : TRECT;
        ScreenW, ScreenH,
        X, Y,
        PopupW, PopupH,
        ParentW, ParentH : INTEGER;

    PROCEDURE SetupValues(Wnd : HWND; VAR R : TRECT; VAR W, H : INTEGER);
        (*local proc to setup the values used *)
     BEGIN (*setupvalues*)
      GetWindowRect(Wnd,R);
      W := R.Right - R.Left;
      H := R.Bottom - R.Top;
     END; (*SetupValues*)

    PROCEDURE SetupLocation(PopupSize, ScreenSize, ParentSize,
                            ParentStart : INTEGER; VAR PopupStart : INTEGER);
        (*local proc to setup location for pop-up *)
     BEGIN (*setuplocation*)
      IF PopupSize > ScreenSize
       THEN PopupStart := 0
      ELSE BEGIN {set up start pos}
            IF PopupSize <= ParentSize
             THEN PopupStart := ParentStart+((ParentSize-PopupSize) DIV 2)
            ELSE PopupStart := ParentStart-((PopupSize-ParentSize) DIV 2);
            IF PopupStart > ScreenSize
             THEN PopupStart := 0
            ELSE IF PopupStart+PopupSize > ScreenSize
                  THEN PopupStart := ScreenSize-PopupSize;
           END; {else}
     END; (*SetupLocation*)

  BEGIN (*CenterPopup*)
   ScreenW := Screen.Width; ScreenH := Screen.Height;
   PopupW := aPopup.Width; PopupH := aPopup.Height;
   SetupValues(aParent,ParentR,ParentW,ParentH);
   SetupLocation(PopupW,ScreenW,ParentW,ParentR.Left,X);
   SetupLocation(PopupH,ScreenH,ParentH,ParentR.Top,Y);
   aPopup.Left := X; aPopup.Top := Y;
  END; (*CenterPopup*)

{**************************************************************************}

END. (*of unit*)
