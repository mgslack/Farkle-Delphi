unit FarkCnst;

(*                                                                      *)
(* Unit defines the global constants and types for use in the game of   *)
(* Farkle.                                                              *)
(*                                                                      *)

interface

 USES Classes, Graphics, Messages;

 CONST PlayerNameLen  = 20;

 TYPE CPLAYSTR = STRING[PlayerNameLen];

 CONST MaxPlayers     = 6;
       MaxCompNames   = 9;
       NumDice        = 6;
       PlayerDispFmt  = '%-21s%7d';
       SafetyScoreDif = 200;
       DefMinScore    = 500;   // if strict scoring on
       DefWinScore    = 10000;
       NumFiles       = 2;
       F_Roll_Snd     = 1;
       F_Farkle_Snd   = 2;
       CompPlayerNms  : ARRAY[1..MaxCompNames] OF CPLAYSTR = (
                         'Telly Belly', 'Lucky Ducky', 'Moody Boody',
                         'Nuckle Buckle', 'Jokey Lokey', 'DumbA$$',
                         'Roofus Doofus', 'Jacky Macky', 'Howdy Dowdy');
       DefPlayerColor : TColor = clSilver;
       CurPlayerColor : TColor = clWhite;
       FirstPlayerClr : TColor = clOlive;
       WonPlayerColor : TColor = clLime;
       DiceColor      : TColor = clWhite;
       NoDiceColor    : TColor = clTeal;
       // custom messages
       wm_StartRound  = wm_User + 101;
       wm_CompMove    = wm_User + 102;
       wm_NextPlayer  = wm_User + 103;
       sc_About       = wm_User + 104;
       // messages used in more than one place
       BadDiceMsg = 'You''ve kept dice that aren''t scoring dice.'#10 +
                    'Please remove before rolling or ending turn.';
       F_Msg      = 'Farkle!';
       SoundFns   : ARRAY[1..NumFiles] OF STRING[50] = ('farkle_rolldice.wav',
                                                        'farkle_farkle.wav');
       NamesFn    : STRING[16] = 'farkle_names.txt';

 TYPE PLAYER_ARRAY = ARRAY[1..MaxPlayers] OF CPLAYSTR;
      TDICE_ARRAY  = ARRAY[1..NumDice] OF INTEGER;

 VAR CompPlyrNames : TSTRINGLIST; {created/destroyed in Fark_Win}

(************************************************************************)

implementation

(************************************************************************)

end.
