import Summit.IO
import Summit.Test

import qualified Game.Vector as Vector
import qualified Game.Movement as Movement
import qualified Util.Range as Range
import qualified Test.Math as Math
import qualified Physics.Friction as Friction
import qualified Util.ID as ID
import qualified Util.Graph as Graph

main :: SystemIO ()
main = defaultMain
        [ Vector.test
        , Movement.test
        , Range.test
        , Math.test
        , Friction.test
        , ID.test
        , Graph.test
        ]
