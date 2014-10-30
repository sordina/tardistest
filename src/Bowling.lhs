> {-# LANGUAGE DoRec #-}
> import Control.Monad.Tardis

Let's first define a data type that captures
the essence of a bowling game.
A game consists of 10 "frames".
Although we model a single `Frame` as a data type,
there are special rules that apply to the final frame,
so we will model it separately as `LFrame`.

> data BowlingGame = BowlingGame
>   { frames :: [Frame]  -- should be 9, too tedious to type restrict
>   , lastFrame :: LFrame }
>
> data Frame = Strike
>            | Spare { firstThrow :: Int }
>            | Frame { firstThrow, secondThrow :: Int }
>
> data LFrame = LStrike { bonus1, bonus2 :: Int }
>             | LSpare { throw1, bonus1 :: Int }
>             | LFrame { throw1, throw2 :: Int }

For details on how bowling is scored, see
[Wikipedia > Bowling # Scoring](http://en.wikipedia.org/wiki/Bowling#Scoring).

Sample data
======================================================================

Here's a few games' worth of sample bowling data.

> --    X  9/ X  X  X   81  7/  X   X   XXX
> -- 0  20 40 70 98 117 126 146 176 206 236
> -- this guy is really good.
> sampleGame = BowlingGame
>   { frames =
>     [ Strike    , Spare 9
>     , Strike    , Strike
>     , Strike    , Frame 8 1
>     , Spare 7   , Strike
>     , Strike
>     ]
>   , lastFrame = LStrike 10 10
>   }
>
> perfectGame = BowlingGame
>   { frames = replicate 9 Strike
>   , lastFrame = LStrike 10 10
>   }
>
> worstGame = BowlingGame
>   { frames = replicate 9 (Frame 0 0)
>   , lastFrame = LFrame 0 0
>   }
>
> main = mapM_ (print . toScores) [sampleGame, perfectGame, worstGame]

Using a Tardis
======================================================================

Well now we want to write the function `toScores :: BowlingGame -> [Int]`.

> newtype PreviousScores = PreviousScores [Int]
> newtype NextThrows = NextThrows (Int, Int)

Here I've chosen the newtype `PreviousScores` for the *forwards* state,
(because coming from the past to the present is moving "forwards" in time)
and `NextThrows` as the *backwards* state
(because coming from the future to the present is moving "backwards" in time).

> toScores :: BowlingGame -> [Int]
> toScores game = flip evalTardis initState $ go (frames game) where
>   go :: [Frame] -> Tardis NextThrows PreviousScores [Int]

First, we handle the case where we have another frame to process.
We begin by assuming we have access to the next two throws
(`nextThrow1` and `nextThrow2`), as well as the previous `score`.

>   go (f : fs) = do
>     rec
>       let (score', throws') = case f of
>             Strike    -> (score + 10 + nextThrow1 + nextThrow2, (10, nextThrow1))
>             Spare n   -> (score + 10 + nextThrow1,              (n, 10 - n))
>             Frame n m -> (score + n + m,                        (n, m))

We need to determine the new state for each of the two
streams of state.

Now that we've got that figured out, we just use the
tardis's capabilities in order to retrieve and send
information along its correct time stream.

>       sendPast $ NextThrows throws'
>       PreviousScores scores@(score : _) <- getPast
>       sendFuture $ PreviousScores (score' : scores)
>       NextThrows ~(nextThrow1, nextThrow2) <- getFuture

Great! Finally, we move on to the rest of the frames.

>     go fs

Once we run out of frames, we need to handle the last frame.
There is no future to be concerned about,
and we can just set up the values to be sent to the recent past
via `initState`, so all we have to do is look at the past score,
add the final frame's score, and we're done.

>   go [] = do
>     PreviousScores scores@(score : _) <- getPast
>     return $ (finalFrameScore + score) : scores

All that's left is to figure out how to determine
the final frame's score, as well as the initial state.

>   finalFrameScore = case lastFrame game of
>     LStrike b1 b2 -> 10 + b1 + b2
>     LSpare  t1 b1 -> 10 + b1
>     LFrame  t1 t2 -> t1 + t2

The "initial state" fed into a tardis is the
*farthest past* for the *forwards-travelling* state,
and the *farthest future* for the *backwards-travelling* state.

>   initState = (NextThrows $ case lastFrame game of
>     LStrike b1 b2 -> (10, b1)
>     LSpare t1 _b1 -> (t1, 10 - t1)
>     LFrame t1 t2  -> (t1, t2)
>     , PreviousScores [0])

And... that's it! All we had to do was encode the rules of Bowling
into a Tardis, and via some timey-wimey trickery,
the Tardis assembles all of the information into
a list of bowling scores, from the last frame to the first.

    [ghci]
    main
