UNIT FarkGame;

(*                                                                      *)
(* Unit defines Farkle game utility methods along with other functions  *)
(* used to play the game (computer player).                             *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2013-01-07 - Added additional function (GetTotalScore) to   *)
(*                       check, if safety is on, if you have the most   *)
(*                       score possible in kept and rolled dice.        *)
(*          2020-06-20 - Fixed bug in comp score 6 method.  Was not     *)
(*                       checking for last set of three of a kinds.     *)
(*                                                                      *)

INTERFACE

 USES FarkCnst;

(************************************************************************)

 PROCEDURE RollDice(VAR Dice : TDICE_ARRAY; NumToRoll : INTEGER);
     (* procedure used to roll the dice and load an array *)

 PROCEDURE RollDiceSorted(VAR Dice : TDICE_ARRAY; NumToRoll : INTEGER);
     (* procedure used to roll the dice and load an array - in sorted order *)

 FUNCTION  GetKeptScore(Dice : TDICE_ARRAY; FourTwo : BOOLEAN) : INTEGER;
     (* function used to return the running score of a set of Farkle dice *)

 FUNCTION  GetTotalScore(RolledDice, KeptDice : TDICE_ARRAY;
                         FourTwo : BOOLEAN) : INTEGER;
     (* function used to get the total score between kept and rolled dice *)
     (* - used only for 'safe' mode checking to make sure user isn't      *)
     (*   throwing away significant points                                *)
	 
 FUNCTION  ValidateKeptDice(Dice : TDICE_ARRAY; FourTwo : BOOLEAN) : BOOLEAN;
     (* function to make sure no junk was moved to the kept dice area *)

 PROCEDURE PlayCompMove(Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                        VAR RollAgain : BOOLEAN; VAR KScore : INTEGER;
                        N, FPW, WP : INTEGER; StrictF, FT_Flg : BOOLEAN);
     (* procedure to play a roll for computer *)

(************************************************************************)

IMPLEMENTATION

 USES SysUtils, Dice;

 CONST CM_MinPoints  = 300;  // minimum points to stop at if no default minimum
       CM_StopPoints = 1000; // stop rolling if greater than this score

(************************************************************************)

 PROCEDURE RollDice(VAR Dice : TDICE_ARRAY; NumToRoll : INTEGER);

    VAR I : INTEGER;

  BEGIN
   // zero array first
   FOR I := 1 TO NumDice DO
    Dice[I] := 0;
   // roll up to the number of dice wanted
   FOR I := 1 TO NumToRoll DO
    Dice[I] := Random(6) + 1;
  END;

(************************************************************************)

 PROCEDURE SortDice(VAR Dice : TDICE_ARRAY);

    VAR Temp, I, J, K : INTEGER;

  BEGIN
   FOR I := 2 TO NumDice DO
    BEGIN
     J := 1;
     WHILE (J < I) DO
      BEGIN
       IF Dice[I] < Dice[J]
        THEN BEGIN
              Temp := Dice[I];
              FOR K := I-1 DOWNTO J DO
               Dice[K+1] := Dice[K];
              Dice[J] := Temp;
             END;
       Inc(J);
      END;
    END;
  END;

(************************************************************************)

 PROCEDURE RollDiceSorted(VAR Dice : TDICE_ARRAY; NumToRoll : INTEGER);

     VAR I : INTEGER;

  BEGIN
   RollDice(Dice, NumToRoll);
   FOR I := NumToRoll+1 TO NumDice DO
    Dice[I] := 99;
   SortDice(Dice);
   FOR I := NumToRoll+1 TO NumDice DO
    Dice[I] := 0;
  END;

(************************************************************************)


 FUNCTION ScoreOneOrFive(Die : INTEGER) : INTEGER;

  BEGIN
   Result := 0;
   IF Die = 1 THEN Result := 100 ELSE IF Die = 5 THEN Result := 50;
  END;

(************************************************************************)

 FUNCTION ScoreThreeDice(Dice : TDICE_ARRAY) : INTEGER;

    VAR I : INTEGER;

  BEGIN
   Result := 0;
   IF (Dice[1] = Dice[2]) AND (Dice[2] = Dice[3])
    THEN BEGIN // 3 of a kind
          IF Dice[1] = 1 THEN Result := 300 ELSE Result := Dice[1] * 100;
         END
   ELSE BEGIN // 1's and 5's only...
         FOR I := 1 TO 3 DO
          Result := Result + ScoreOneOrFive(Dice[I]);
        END;
  END;

(************************************************************************)

 FUNCTION ScoreFourDice(Dice : TDICE_ARRAY) : INTEGER;

    VAR I, C1 : INTEGER;

  BEGIN
   Result := 0;
   IF (Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND (Dice[3] = Dice[4])
    THEN BEGIN
          Result := 1000;
         END
   ELSE BEGIN // check three of a kind / 1 and 5
         IF ((Dice[1] = Dice[2]) AND (Dice[2] = Dice[3])) OR
            ((Dice[2] = Dice[3]) AND (Dice[3] = Dice[4]))
          THEN BEGIN // 3 of a kind
                IF (Dice[2] = 1) THEN Result := 300 ELSE Result := Dice[2] * 100;
                IF Dice[1] = Dice[2]
                 THEN C1 := Dice[4]
                ELSE C1 := Dice[1];
                Result := Result + ScoreOneOrFive(C1);
               END
         ELSE BEGIN // 1's and 5's only
               FOR I := 1 TO 4 DO
                Result := Result + ScoreOneOrFive(Dice[I]);
              END;
        END;
  END;

(************************************************************************)

 FUNCTION ScoreFiveDice(Dice : TDICE_ARRAY) : INTEGER;

    VAR I, C1, C2 : INTEGER;

  BEGIN
   Result := 0;
   IF (Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND
      (Dice[3] = Dice[4]) AND (Dice[4] = Dice[5])
    THEN BEGIN
          Result := 2000;
         END
   ELSE BEGIN // check three/four of a kind / 1 and 5
         IF ((Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND
             (Dice[3] = Dice[4])) OR
            ((Dice[2] = Dice[3]) AND (Dice[3] = Dice[4]) AND
             (Dice[4] = Dice[5]))
          THEN BEGIN // 4 of a kind
                Result := 1000;
                IF Dice[1] = Dice[2] THEN C1 := Dice[5] ELSE C1 := Dice[1];
                Result := Result + ScoreOneOrFive(C1);
               END
         ELSE BEGIN // 3 of a kind with others then
               IF ((Dice[1] = Dice[2]) AND (Dice[2] = Dice[3])) OR
                  ((Dice[2] = Dice[3]) AND (Dice[3] = Dice[4])) OR
                  ((Dice[3] = Dice[4]) AND (Dice[4] = Dice[5]))
                THEN BEGIN // 3 of a kind
                      IF (Dice[3] = 1)
                       THEN Result := 300
                      ELSE Result := Dice[3] * 100;
                      IF (Dice[1] = Dice[3])
                       THEN BEGIN
                             C1 := Dice[4]; C2 := Dice[5];
                            END
                      ELSE IF (Dice[3] = Dice[5])
                            THEN BEGIN
                                  C1 := Dice[1]; C2 := Dice[2];
                                 END
                           ELSE BEGIN
                                 C1 := Dice[1]; C2 := Dice[5];
                                END;
                      Result := Result + ScoreOneOrFive(C1) + ScoreOneOrFive(C2);
                     END
               ELSE BEGIN // 1's and 5's only (plus junk?)
                     FOR I := 1 TO 5 DO
                      Result := Result + ScoreOneOrFive(Dice[I]);
                    END;
              END;
        END;
  END;

(************************************************************************)

 FUNCTION ScoreSixDice(Dice : TDICE_ARRAY; FT_Flg : BOOLEAN) : INTEGER;

    VAR I, C1, C2, C3 : INTEGER;

  BEGIN
   Result := 0;
   IF (Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND
      (Dice[3] = Dice[4]) AND (Dice[4] = Dice[5]) AND
      (Dice[5] = Dice[6])
    THEN BEGIN // 6 of a kind
          Result := 3000;
         END
   ELSE IF (Dice[1] = Dice[2]) AND (Dice[3] = Dice[4]) AND
           (Dice[5] = Dice[6])
         THEN BEGIN // 3 pair (or 4 of a kind plus pair)
               IF ((Dice[2] = Dice[3]) OR (Dice[4] = Dice[5])) AND NOT(FT_Flg)
                THEN BEGIN
                      Result := 1000;
                      IF Dice[2] = Dice[3]
                       THEN BEGIN
                             C1 := Dice[5]; C2 := Dice[6];
                            END
                      ELSE BEGIN
                            C1 := Dice[1]; C2 := Dice[2];
                           END;
                      Result := Result + ScoreOneOrFive(C1) + ScoreOneOrFive(C2);
                     END
               ELSE Result := 1500;
              END
   ELSE IF ((Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND
            (Dice[3] = Dice[4]) AND (Dice[4] = Dice[5])) OR
           ((Dice[2] = Dice[3]) AND (Dice[3] = Dice[4]) AND
            (Dice[4] = Dice[5]) AND (Dice[5] = Dice[6]))
         THEN BEGIN // 5 of a kind
               Result := 2000;
               IF Dice[1] = Dice[2]
                THEN C1 := Dice[6]
               ELSE C1 := Dice[1];
               Result := Result + ScoreOneOrFive(C1);
              END
   ELSE IF (Dice[1] = 1) AND (Dice[2] = 2) AND (Dice[3] = 3) AND
           (Dice[4] = 4) AND (Dice[5] = 5) AND (Dice[6] = 6)
         THEN BEGIN // straight
               Result := 1500;
              END
   ELSE IF (Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND
           (Dice[4] = Dice[5]) AND (Dice[5] = Dice[6])
         THEN BEGIN // 2 triples
               Result := 2500;
              END
   ELSE BEGIN // 3 or 4 of a kind with 1's and 5's around?
         IF ((Dice[1] = Dice[2]) AND (Dice[2] = Dice[3]) AND
             (Dice[3] = Dice[4])) OR
            ((Dice[2] = Dice[3]) AND (Dice[3] = Dice[4]) AND
             (Dice[4] = Dice[5])) OR
            ((Dice[3] = Dice[4]) AND (Dice[4] = Dice[5]) AND
             (Dice[5] = Dice[6]))
          THEN BEGIN // 4 of a kind
                Result := 1000;
                IF (Dice[2] <> Dice[3])
                 THEN BEGIN
                       C1 := Dice[1]; C2 := Dice[2];
                      END
                ELSE IF (Dice[4] <> Dice[5])
                      THEN BEGIN
                            C1 := Dice[5]; C2 := Dice[6];
                           END
                     ELSE BEGIN
                           C1 := Dice[1]; C2 := Dice[6];
                          END;
                Result := Result + ScoreOneOrFive(C1) + ScoreOneOrFive(C2);
               END
         ELSE IF ((Dice[2] = Dice[3]) AND (Dice[3] = Dice[4])) OR
                 ((Dice[3] = Dice[4]) AND (Dice[4] = Dice[5])) OR
                 ((Dice[4] = Dice[5]) AND (Dice[5] = Dice[6]))
               THEN BEGIN // 3 of a kind plus 1's and 5's
                     // note, first die cannot be part of a three of a
                     // kind for 6 valid dice
                     Result := Dice[4] * 100;
                     C1 := Dice[1];
                     IF Dice[2] = Dice[4]
                      THEN BEGIN
                            C2 := Dice[5]; C3 := Dice[6];
                           END
                     ELSE IF Dice[3] = Dice[5]
                           THEN BEGIN
                                 C2 := Dice[2]; C3 := Dice[6];
                                END
                          ELSE BEGIN
                                C2 := Dice[2]; C3 := Dice[3];
                               END;
                     Result := Result + ScoreOneOrFive(C1) + ScoreOneOrFive(C2) +
                                        ScoreOneOrFive(C3);
                    END
         ELSE BEGIN // 1's and 5's plus junk?
               FOR I := 1 TO 6 DO
                Result := Result + ScoreOneOrFive(Dice[I]);
              END;
        END;
  END;

(************************************************************************)

 FUNCTION GetKeptScore(Dice : TDICE_ARRAY; FourTwo : BOOLEAN) : INTEGER;

    VAR I, N : INTEGER;

  BEGIN
   Result := 0; N := 0;
   FOR I := 1 TO NumDice DO
    IF Dice[I] = 0 THEN Dice[I] := 99 ELSE Inc(N);
   IF N <> 0
    THEN BEGIN
          SortDice(Dice);
          CASE N OF
           3 : Result := ScoreThreeDice(Dice);
           4 : Result := ScoreFourDice(Dice);
           5 : Result := ScoreFiveDice(Dice);
           6 : Result := ScoreSixDice(Dice, FourTwo);
           ELSE BEGIN {only 1's and 5's?}
                 Result := ScoreOneOrFive(Dice[1]) + ScoreOneOrFive(Dice[2]);
                END;
          END;
         END;
  END;

(************************************************************************)

 FUNCTION GetTotalScore(RolledDice, KeptDice : TDICE_ARRAY;
                        FourTwo : BOOLEAN) : INTEGER;
 
    VAR Dice : TDICE_ARRAY;
        I    : INTEGER;
 
  BEGIN (*gettotalscore*)
   FOR I := 1 TO NumDice DO
    IF RolledDice[I] <> 0
     THEN Dice[I] := RolledDice[I]
    ELSE Dice[I] := KeptDice[I];
   Result := GetKeptScore(Dice, FourTwo);
  END; (*gettotalscore*)

(************************************************************************)

 FUNCTION ValidateKeptDice(Dice : TDICE_ARRAY; FourTwo : BOOLEAN) : BOOLEAN;

    VAR I, N, C : INTEGER;
        ChkA    : ARRAY[1..6] OF INTEGER;

  BEGIN
   Result := TRUE; N := 0;
   FOR I := 1 TO 6 DO
    ChkA[I] := 0;
   FOR I := 1 TO NumDice DO
    BEGIN
     IF Dice[I] <> 0
      THEN BEGIN
            Inc(N); Inc(ChkA[Dice[I]]);
           END;
    END;
   // only need to check if have dice selected and have 2's, 3's, 4's and 6's
   IF (N <> 0) AND
      ((ChkA[2] <> 0) OR (ChkA[3] <> 0) OR (ChkA[4] <> 0) OR (ChkA[6] <> 0))
    THEN BEGIN
          IF (N <> 6) AND
             (((ChkA[2] > 0) AND (ChkA[2] < 3)) OR
              ((ChkA[3] > 0) AND (ChkA[3] < 3)) OR
              ((ChkA[4] > 0) AND (ChkA[4] < 3)) OR
              ((ChkA[6] > 0) AND (ChkA[6] < 3)))
           THEN Result := FALSE;
          IF N = 6
           THEN BEGIN
                 C := 0;
                 FOR I := 1 TO 6 DO
                  IF ChkA[I] = 2 THEN Inc(C);
                 IF (C <> 3) AND (C = 1)
                  THEN FOR I := 1 TO 6 DO
                        IF (ChkA[I] = 4) AND (FourTwo) THEN C := 3;
                 IF C <> 3
                  THEN BEGIN // not three pair, check for triplets
                        C := 0;
                        FOR I := 1 TO 6 DO
                         IF ChkA[I] = 3 THEN Inc(C);
                        IF C <> 2   // not two triplets
                         THEN BEGIN // check for straight
                               C := 0;
                               FOR I := 1 TO 6 DO
                                IF ChkA[I] = 1 THEN Inc(C);
                               IF C <> 6
                                THEN BEGIN // check for 3,4,5 of a kind
                                      IF ((ChkA[2] > 0) AND (ChkA[2] < 3)) OR
                                         ((ChkA[3] > 0) AND (ChkA[3] < 3)) OR
                                         ((ChkA[4] > 0) AND (ChkA[4] < 3)) OR
                                         ((ChkA[6] > 0) AND (ChkA[6] < 3))
                                       THEN Result := FALSE;
                                     END;
                              END;
                       END;
                END;
         END;
  END;

(************************************************************************)

 PROCEDURE CM_ScoreOneDie(Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                          Idx, Pts : INTEGER; VAR KScore, Used : INTEGER);
     (* private comp move proc *)

  BEGIN
   KScore := KScore + Pts;
   Inc(Used);
   Kept[Idx] := Dice[Idx];
  END;

(************************************************************************)

 PROCEDURE CM_ScoreThreeDie(Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                            VAR KScore, Used : INTEGER);
     (* private comp move proc *)

    VAR TS1, TS2, TS3, I : INTEGER;

  BEGIN
   IF ValidateKeptDice(Dice, FALSE)
    THEN BEGIN {all three dice are good}
          KScore := KScore + ScoreThreeDice(Dice);
          Used := 3;
          FOR I := 1 TO 3 DO
           Kept[I] := Dice[I];
         END
   ELSE BEGIN // any scoring dice?
         TS1 := ScoreOneOrFive(Dice[1]);
         TS2 := ScoreOneOrFive(Dice[2]);
         TS3 := ScoreOneOrFive(Dice[3]);
         IF (TS1 + TS2 + TS3) = 0
          THEN KScore := 0
         ELSE BEGIN // at least one die is a scoring die
               IF TS1 > 0 THEN CM_ScoreOneDie(Dice, Kept, 1, TS1, KScore, Used);
               IF TS2 > 0 THEN CM_ScoreOneDie(Dice, Kept, 2, TS2, KScore, Used);
               IF TS3 > 0 THEN CM_ScoreOneDie(Dice, Kept, 3, TS3, KScore, Used);
              END;
        END;
  END;

(************************************************************************)

 PROCEDURE CM_ScoreFourDie(SDice, Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                           VAR KScore, Used : INTEGER);
     (* private comp move proc *)

    VAR ISc, I, Kp : INTEGER;

  BEGIN
   ISc := ScoreFourDice(SDice);
   IF ISc = 0
    THEN KScore := 0 // no score die at all in the four
   ELSE BEGIN
         KScore := KScore + ISc;
         IF ValidateKeptDice(Dice, FALSE)
          THEN BEGIN // all four dice are good
                Used := 4;
                FOR I := 1 TO Used DO
                 Kept[I] := Dice[I];
               END
         ELSE BEGIN // what dice scored then
               Kp := 66;
               IF ((SDice[1] = SDice[2]) AND (SDice[2] = SDice[3])) OR
                  ((SDice[2] = SDice[3]) AND (SDice[3] = SDice[4]))
                THEN Kp := SDice[2]; // had a 3 of a kind
               FOR I := 1 TO 4 DO
                IF (Dice[I] IN [1,5]) OR (Dice[I] = Kp)
                 THEN BEGIN
                       Inc(Used);
                       Kept[I] := Dice[I];
                      END;
              END;
        END;
  END;

(************************************************************************)

 PROCEDURE CM_ScoreFiveDie(SDice, Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                           VAR KScore, Used : INTEGER);
     (* private comp move proc *)

    VAR ISc, I, Kp : INTEGER;

  BEGIN
   ISc := ScoreFiveDice(SDice);
   IF ISc = 0
    THEN KScore := 0 // no score die at all in the five
   ELSE BEGIN
         KScore := KScore + ISc;
         IF ValidateKeptDice(Dice, FALSE)
          THEN BEGIN // all five dice are good
                Used := 5;
                FOR I := 1 TO Used DO
                 Kept[I] := Dice[I];
               END
         ELSE BEGIN // what dice scored then
               Kp := 66;
               IF ((SDice[1] = SDice[2]) AND (SDice[2] = SDice[3]) AND
                   (SDice[3] = SDice[4])) OR
                  ((SDice[2] = SDice[3]) AND (SDice[3] = SDice[4]) AND
                   (SDice[4] = SDice[5])) OR
                  ((SDice[1] = SDice[2]) AND (SDice[2] = SDice[3])) OR
                  ((SDice[2] = SDice[3]) AND (SDice[3] = SDice[4])) OR
                  ((SDice[3] = SDice[4]) AND (SDice[4] = SDice[5]))
                THEN Kp := SDice[3]; // had 3 or 4 of a kind
               FOR I := 1 TO 5 DO
                IF (Dice[I] IN [1,5]) OR (Dice[I] = Kp)
                 THEN BEGIN
                       Inc(Used);
                       Kept[I] := Dice[I];
                      END;
              END;
        END;
  END;

(************************************************************************)

 PROCEDURE CM_ScoreSixDie(SDice, Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                           VAR KScore, Used : INTEGER; FT_Flg : BOOLEAN);
     (* private comp move proc *)

    VAR ISc, I, Kp : INTEGER;

  BEGIN
   ISc := ScoreSixDice(SDice, FT_Flg); Kp := 0;
   // check one special case (three of a kind with first three dice)
   //  - this is not scored in ScoreSixDice since it is not valid
   //    for all six dice normally
   //  - checking for 1's, but really only 2's, 3's and 4's are
   //    the problem
   //  - may have 1 or 2 fives also, so check if ISc is <= 100 too
   IF (ISc <= 100) AND (SDice[1] <> 1) AND
      (SDice[1] = SDice[2]) AND (SDice[2] = SDice[3])
    THEN BEGIN // 3 of a kind and 1 or 2 fives
          Kp := ISc; ISc := 0;
         END;
   IF (ISc = 0) AND (SDice[1] = SDice[2]) AND (SDice[2] = SDice[3])
    THEN BEGIN // have a 3 of a kind leading off the roll
          IF SDice[1] = 1 THEN ISc := 300 ELSE ISc := SDice[1] * 100;
          ISc := ISc + Kp;
         END;
   IF ISc = 0
    THEN KScore := 0 // no score die at all in the six!!!!!
   ELSE BEGIN
         KScore := KScore + ISc;
         IF ValidateKeptDice(Dice, FT_Flg)
          THEN BEGIN // all six dice are good
                Used := 6;
                FOR I := 1 TO Used DO
                 Kept[I] := Dice[I];
               END
         ELSE BEGIN // what dice scored then
               Kp := 66;
               IF ((SDice[1] = SDice[2]) AND (SDice[2] = SDice[3]) AND
                   (SDice[3] = SDice[4]) AND (SDice[4] = SDice[5])) OR
                  ((SDice[2] = SDice[3]) AND (SDice[3] = SDice[4]) AND
                   (SDice[4] = SDice[5]) AND (SDice[5] = SDice[6])) OR
                  ((SDice[1] = SDice[2]) AND (SDice[2] = SDice[3]) AND
                   (SDice[3] = SDice[4])) OR
                  ((SDice[2] = SDice[3]) AND (SDice[3] = SDice[4]) AND
                   (SDice[4] = SDice[5])) OR
                  ((SDice[3] = SDice[4]) AND (SDice[4] = SDice[5]) AND
                   (SDice[5] = SDice[6])) OR
                  ((SDice[1] = SDice[2]) AND (SDice[2] = SDice[3])) OR
                  ((SDice[2] = SDice[3]) AND (SDice[3] = SDice[4])) OR
                  ((SDice[3] = SDice[4]) AND (SDice[4] = SDice[5])) OR
                  ((SDice[4] = SDice[5]) AND (SDice[5] = SDice[6]))
                 THEN BEGIN // had 3, 4 or 5 of a kind
                       Kp := SDice[3];
                       IF (SDice[4] = SDice[5]) AND (SDice[5] = SDice[6])
                        THEN Kp := SDice[4];
                      END;
               // last 3 of a kind case to check (dice[3] not in this one)
               IF (Kp = 66) AND (SDice[4] = SDice[5]) AND (SDice[5] = SDice[6])
                THEN Kp := SDice[5];
               FOR I := 1 TO 6 DO
                IF (Dice[I] IN [1,5]) OR (Dice[I] = Kp)
                 THEN BEGIN
                       Inc(Used);
                       Kept[I] := Dice[I];
                      END;
              END;
        END;
  END;

(************************************************************************)

 PROCEDURE PlayCompMove(Dice : TDICE_ARRAY; VAR Kept : TDICE_ARRAY;
                        VAR RollAgain : BOOLEAN; VAR KScore : INTEGER;
                        N, FPW, WP : INTEGER; StrictF, FT_Flg : BOOLEAN);
     (* procedure will play each roll the computer player makes *)
     (* - decision rules are simple, keep all die that score    *)
     (*   points every roll, throw the rest                     *)
     (* - if no scoring die in roll, stop rolling               *)
     (* - if have 1 or 2 dice left then                         *)
     (*   - if scrict and total greater than default min, stop  *)
     (*     rolling                                             *)
     (*   - if not strict and total greater than CM_MinPoints,  *)
     (*     stop rolling                                        *)
     (* - if total is greater than CM_StopPoints, regardless of *)
     (*   the number of die left, stop rolling                  *)
     (* - if a player has already won, then keep rolling until  *)
     (*   total is greater than needed to beat that player      *)

    VAR I, Used, Left, TS1, TS2 : INTEGER;
        SortedDice              : TDICE_ARRAY;

  BEGIN
   Used := 0;
   FOR I := 1 TO NumDice DO
    Kept[I] := 0;
   IF N > 3
    THEN BEGIN // sort dice to use for scoring
          FOR I := 1 TO NumDice DO
           IF Dice[I] = 0
            THEN SortedDice[I] := 99
           ELSE SortedDice[I] := Dice[I];
           SortDice(SortedDice);
         END;
   // do move
   CASE N OF
    3 : CM_ScoreThreeDie(Dice, Kept, KScore, Used);
    4 : CM_ScoreFourDie(SortedDice, Dice, Kept, KScore, Used);
    5 : CM_ScoreFiveDie(SortedDice, Dice, Kept, KScore, Used);
    6 : CM_ScoreSixDie(SortedDice, Dice, Kept, KScore, Used, FT_Flg);
    ELSE BEGIN // 1 or 2 dice
          TS1 := ScoreOneOrFive(Dice[1]);
          TS2 := ScoreOneOrFive(Dice[2]);
          IF (TS1 + TS2) = 0
           THEN KScore := 0
          ELSE BEGIN
                IF TS1 > 0 THEN CM_ScoreOneDie(Dice, Kept, 1, TS1, KScore, Used);
                IF TS2 > 0 THEN CM_ScoreOneDie(Dice, Kept, 2, TS2, KScore, Used);
               END;
         END;
   END;
   // go again?
   RollAgain := Used > 0;
   IF RollAgain
    THEN BEGIN
          IF FPW > 0
           THEN BEGIN
                 IF  KScore >= WP THEN RollAgain := FALSE;
                END
          ELSE BEGIN
                Left := N - Used;
                IF Left IN [1,2]
                 THEN BEGIN
                       IF StrictF THEN I := DefMinScore ELSE I := CM_MinPoints;
                       RollAgain := KScore < I;
                      END
                ELSE IF (Left >= 3) AND (KScore >= CM_StopPoints)
                      THEN RollAgain := FALSE;
               END;
         END;
  END;

(************************************************************************)

END.

