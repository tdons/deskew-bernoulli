module Math.Deskew  (
      Bit (..)
    , deskew
) where

data Bit = Zero | One
    deriving (Eq, Ord, Show, Read)

-- TODO: make this a parameter.
-- Its first argument denotes the maximal depth to which to grow the tree, leave
-- this at 0 for what is effectively von Neumann's algorith, higher than 10 is
-- not useful for any practical purposes whatsoever.  See "Table 1" in the paper.
maxTreeDepth :: Int
maxTreeDepth = 10

{- | Implementation of the ``random stream algorithm'' as outlined
     in the paper:
     "Streaming Algorithms for Optimal Generation of Random Bits"
       by authors:
     * Hongchao Zhou
     * Jehoshua Bruck

     The paper describes the following:
       i) streaming deskewing algorithm with ∞ tree depth
      ii) variant of i) with bounded tree depth
     iii) a generalization to a multinomial distribution
      iv) extension for Markov chains

      This Haskell module implements only i) and ii).

-}


-- Note: all comments below between """ are quotes from the paper.


{- | Status Tree
     The binary tree that keeps the state of the input stream.
-}
data Tree = Node Label Tree Tree | Leaf

{- | Label
     The label on each Node that holds the state.

     The paper mentions:
     """
     The key idea in our algorithm is to create a binary tree for
     storing the state information, called a status tree. A node
     in the status tree stores a symbol in {φ, H, T, 0, 1}
     """

     I've elected to deviate from this description of the state of
     a node, instead I have grouped the symbols H, T and 0, 1 into
     two groups to make the implementation less cumbersome.
     More concretely:

     H = In One
     T = In Zero
     0 = Out Zero
     1 = Out One

     The authors of the paper mention the passing around of 'messages'
     for which they use H and T, we pass around the messages One and
     Zero (Respectively).

     This diagram uses the nomenclature from the authors, where
     you read 'H' and 'T', this module uses In One and In Zero.  Where
     you read 0 and 1 this module uses Out Zero and Out One.

     +-------------------------+--------------+
     |                         |              |
     v                         |              |
     φ -- a --> H | T          |              |
                H | T -- b --> φ              |
                H | T -- c --> 0 | 1          |
                               0 | 1 -- d --> φ

     This explanation below of the above diagram uses the nomenclature
     used throughout this program.  The hope is that it will function
     as an aid to understanding the code in this modue and to map it to
     the paper as best as possible.  Each state transition is the result
     of a message arriving at a node.

     a) One arrives at node ->
            state <- In One
        Zero arrives at node ->
            state <- In Zero
     b) One arrives at node with state In One ->
            state <- φ
            send Zero to left child
            send One to right child
        Zero arrives at node with state In Zero ->
            state <- φ
            send Zero to left and right child
     c) One arrives at node with state In Zero ->
            state <- One
            send One to left child
        Zero arrives at node with state In One ->
            state <- Zero
            send One to left child
     d) m = One | Zero arrives at node with state s = Out One | Out Zero ->
            emit output bit s
            state <- In m

     State is modulated by way of 'input bits', as you can see
     from the diagram the H and T states are arrived at first, from
     there the transition to 0 and 1 is made, after which an output
     bit is produced and we're back at the empty state.  Hence the
     names "In" and "Out".
-}
data Label = Empty   -- φ
           | In Bit  -- T and H represented by Zero, One respectively
           | Out Bit -- 0 and 1 represented by Zero, One respectively

-- | An empty status tree
newStatusTree :: Tree
newStatusTree = Node Empty Leaf Leaf

-- | Deskews a list of bits.  In other words it removes
--   the *bias*, if present, in the sequence of input bits.
--
--   That is to say: let the input be a list of independently
--   and identically distributed variables drawn from a Bernoulli
--   distribution with unknown p.
--
--   This function will then transform this list to one that
--   * contains at most as many elements as the input list
--   * contains variables that are distributed according to a
--     Bernoulli distribution with p = 0.5
--   * Works in a streaming fashion, that is: output bits are
--     generated on the fly as the input list is being processed
--   * achieves close to the theoretical optimum if the maximum
--     tree depth is set at approx. ~10.  If set to infinity the
--     theoretical optimum is achieved, however the memory costs
--     are prohibitive for some inputs (e.g.: "repeat Zero")
deskew :: [Bit] -> [Bit]
deskew = deskew' newStatusTree

deskew' :: Tree -> [Bit] -> [Bit]
deskew' t (ib:ibs) = ob' ++ (deskew' t' ibs)
    where (t', ob') = transformTree t ib
deskew' _ _ = []

{- | Transforms the statustree after receiving an input bit.  The result of this
     operation is a new statustree and a list of output bits.
-}
transformTree :: Tree -> Bit -> (Tree, [Bit])
transformTree = passSymbol 0

{- | The guts of the algorithm -}
passSymbol :: Int           -- | The depth of the current node
           -> Tree          -- | Status tree
           -> Bit           -- | Input symbol
           -> (Tree, [Bit]) -- | New status tree, output bits

passSymbol d node sym =
  case node of
    -- (Continued below, see line with "Continuation")
    -- """
    -- 3) When x = H or T, we first check whether u has children.
    --    If it does not have, we create two children with label φ for
    --    it.  Let u_l and u_r denote the two children of u.
    -- """
    Leaf ->
      case () of
        _ | d < maxTreeDepth -> passSymbol d newStatusTree sym
          | otherwise        -> (Leaf, [])
    Node label l r ->
      case (label, sym) of
        -- """ 1) When x = φ, set x = y. """
        (Empty,   _)    -> (Node (In sym)   l  r, [])

        -- """ 2) When x = 1 or 0, output x and set x = y. """
        (Out bit, _)    -> (Node (In sym)   l  r, [bit])

        -- (Continuation of 3) below this line)
        --    """
        --    * If xy = HH, we set x = φ, then pass a symbol T to u_l and
        --      a symbol H to u_r.
        --    """
        (In One,  One)  -> passSymbolToChildren d node Zero One

        --    """
        --    * If xy = TT, we set x = φ, then pass a symbol T to u_l and
        --      a symbol T to u_r.
        --    """
        (In Zero, Zero) -> passSymbolToChildren d node Zero Zero

        --    """ * If xy = HT, we set x = 1, then pass a symbol H to u_l. """
        (In One,  Zero) -> (Node (Out One)  lt r, lb)

        --    """ * If xy = TH, we set x = 0, then pass a symbol H to u_l. """
        (In Zero, One)  -> (Node (Out Zero) lt r, lb)

        where (lt, lb) = passSymbol (d + 1) l One

{- | Utility function to pass messages to both the left and the right
     child of a node.  The function constructs the new status tree and
     returns the concatenated results (output bits) if any.
-}
passSymbolToChildren :: Int -- | Tree depth
                    -> Tree -- | The *node* to whose children we message
                    -> Bit  -- | Message for the left child
                    -> Bit  -- | Message for the right child
                    -> (Tree, [Bit]) -- | New status tree and output bits
passSymbolToChildren d (Node _ l r) ls rs = (Node Empty lt rt, lb ++ rb)
    where (lt, lb) = passSymbol (d + 1) l ls
          (rt, rb) = passSymbol (d + 1) r rs

-- | We include this to silence GHC.  The case expressions inside the
--   function passSymbol will ensure that this never happens.
--
--   Note: this function is not exported.
--
passSymbolToChildren _ Leaf _ _ =
    error "Math.Deskew passSymbolToChildren called on Leaf. Should not happen."

