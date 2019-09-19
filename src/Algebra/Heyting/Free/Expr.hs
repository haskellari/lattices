{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algebra.Heyting.Free.Expr (
    Expr (..),
    proofSearch,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Monad             (ap)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Data                 (Data, Typeable)
import Data.Set                  (Set)
import GHC.Generics              (Generic, Generic1)

import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Expr
-------------------------------------------------------------------------------

-- | Heyting algebra expression.
--
-- /Note:/ this type doesn't have 'Algebra.Heyting.Heyting' instance,
-- as its 'Eq' and 'Ord' are structural.
--
data Expr a
    = Var a
    | Bottom
    | Top
    | Expr a :/\: Expr a
    | Expr a :\/: Expr a
    | Expr a :=>: Expr a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

infixr 6 :/\:
infixr 5 :\/:
infixr 4 :=>:

instance Applicative Expr where
    pure = Var
    (<*>) = ap

instance Monad Expr where
    return = pure

    Var x      >>= k = k x
    Bottom     >>= _ = Bottom
    Top        >>= _ = Top
    (x :/\: y) >>= k = (x >>= k) :/\: (y >>= k)
    (x :\/: y) >>= k = (x >>= k) :\/: (y >>= k)
    (x :=>: y) >>= k = (x >>= k) :=>: (y >>= k)

-------------------------------------------------------------------------------
-- LJT proof search
-------------------------------------------------------------------------------

-- | Decide whether @x :: 'Expr' a@ is provable.
--
-- /Note:/ this doesn't construct a proof term, but merely returns a 'Bool'.
--
proofSearch :: forall a. Ord a => Expr a -> Bool
proofSearch tyGoal = evalState (emptyCtx |- fmap R tyGoal) 0
  where
    freshVar = do
        n <- get
        put (n + 1)
        return (L n)

    infix 4 |-
    infixr 3 .&&

    (.&&) :: Monad m => m Bool -> m Bool -> m Bool
    x .&& y = do
        x' <- x
        if x'
        then y
        else return False

    (|-) :: Ctx a -> Expr (Am a) -> State Int Bool

    -- Ctx ats ai ii xs |- _
    --     | traceShow (length ats, length ai, length ii, length xs) False
    --     = return False

    -- T-R
    _ctx |- Top
        = return True

    -- T-L
    Ctx ats ai ii (Top : ctx) |- ty
        = Ctx ats ai ii ctx |- ty

    -- F-L
    Ctx _ _ _ (Bottom : _ctx) |- _ty
        = return True

    -- Id-atoms
    Ctx ats _ai _ii [] |- Var a
        | Set.member a ats
        = return True

    -- Id
    Ctx _ats _ai _ii (x : _ctx) |- ty
        | x == ty
        = return True

    -- Move atoms to atoms part of context
    Ctx ats ai ii (Var a : ctx) |- ty
        = Ctx (Set.insert a ats) ai ii ctx |- ty

    -- =>-R
    Ctx ats ai ii ctx |- (a :=>: b)
        = Ctx ats ai ii (a : ctx) |- b

    -- /\-L
    Ctx ats ai ii ((x :/\: y) : ctx) |- ty
        = Ctx ats ai ii (x : y : ctx) |- ty

    -- =>-L-extra (Top)
    --
    -- \Gamma, C      |- G
    -- --------------------------
    -- \Gamma, 1 -> C |- G
    --
    Ctx ats ai ii ((Top :=>: c) : ctx) |- ty
        = Ctx ats ai ii (c : ctx) |- ty

    -- =>-L-extra (Bottom)
    --
    -- \Gamma         |- G
    -- --------------------------
    -- \Gamma, 0 -> C |- G
    --
    Ctx ats ai ii ((Bottom :=>: _) : ctx) |- ty
        = Ctx ats ai ii ctx |- ty

    -- =>-L2 (Conj)
    --
    -- \Gamma, A -> (B -> C) |- G
    -- --------------------------
    -- \Gamma, (A /\ B) -> C |- G
    --
    Ctx ats ai ii ((a :/\: b :=>: c) : ctx) |- ty
        = Ctx ats ai ii ((a :=>: b :=>: c) : ctx) |- ty

    -- =>-L3 (Disj)
    --
    -- \Gamma, A -> C, B -> C |- G
    -- ---------------------------
    -- \Gamma, (A \/ B) -> C  |- G
    --
    -- or with fresh var: (P = A \/ B, but an atom)
    --
    -- \Gamma, A -> P, B -> P, P -> C |- G
    -- -----------------------------------
    -- \Gamma, (A \/ B) -> C          |- G
    --
    Ctx ats ai ii ((a :\/: b :=>: c) : ctx) |- ty = do
        p <- Var <$> freshVar
        Ctx ats ai ii ((p :=>: c) : (a :=>: p) : (b :=>: p) : ctx) |- ty

    -- =>-L4 preparation
    --
    -- \Gamma, B -> C, A |- B    \Gamma, C |- G
    -- ------------------------------------------
    -- \Gamma, (A -> B) -> C |- G
    --
    Ctx ats ai ii (((a :=>: b) :=>: c) : ctx) |- ty
        = Ctx ats ai (Set.insert (ImplImpl a b c) ii) ctx |- ty

    -- =>-L1 preparation
    --
    -- \Gamma, X, B      |- G
    -- ----------------------
    -- \Gamma, X, X -> B |- G
    --
    Ctx ats ai ii ((Var x :=>: b) : ctx) |- ty
        = Ctx ats (Set.insert (AtomImpl x b) ai) ii ctx |- ty

    -- These two rules, (\/-L) and (/\-R), are pushed to the last, as they branch.

    -- \/-L
    Ctx ats ai ii ((x :\/: y) : ctx) |- ty
        =   Ctx ats ai ii (x : ctx) |- ty
        .&& Ctx ats ai ii (y : ctx) |- ty

    -- /\-R
    ctx |- (a :/\: b)
        =   ctx |- a
        .&& ctx |- b

    -- Last rules
    Ctx ats ai ii [] |- ty
        -- L1 completion
        | ((y, ai') : _) <- match
        = Ctx ats ai' ii [y] |- ty

        -- \/-R and =>-L4
        | not (null rest) = iter rest
      where
        match =
            [ (y, Set.delete ai' ai)
            | ai'@(AtomImpl x y) <- Set.toList ai
            , x `Set.member` ats
            ]

        -- try in order
        iter [] = return False
        iter (Right (ctx', ty') : rest') = do
            res <- ctx' |- ty'
            if res
            then return True
            else iter rest'

        iter (Left (ctxa, a, ctxb, b) : rest') = do
            res <- ctxa |- a .&& ctxb |- b
            if res
            then return True
            else iter rest'

        rest = disj ++ implImpl

        -- =>-L4
        implImpl =
            [ Left (Ctx ats ai ii' [x, y :=>: z], y, Ctx ats ai ii' [z], ty)
            | entry@(ImplImpl x y z) <- Set.toList ii
            , let ii' = Set.delete entry ii
            ]

        -- \/-R
        disj = case ty of
            a :\/: b ->
                [ Right (Ctx ats ai ii [], a)
                , Right (Ctx ats ai ii [], b)
                ]
            _ -> []

    Ctx _ _ _ [] |- (_ :\/: _)
        = error "panic! @proofSearch should be matched before"

    Ctx _ _ _ [] |- Var _
        = return False

    Ctx _ _ _ [] |- Bottom
        = return False

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Am a
    = L !Int
    | R a
  deriving (Eq, Ord, Show)

data Ctx a = Ctx
    { ctxAtoms      :: Set (Am a)
    , ctxAtomImpl   :: Set (AtomImpl a)
    , ctxImplImpl   :: Set (ImplImpl a)
    , ctxHypothesis :: [Expr (Am a)]
    }
  deriving Show

emptyCtx :: Ctx l
emptyCtx = Ctx Set.empty Set.empty Set.empty []

-- [[ AtomImpl a b ]] = a => b
data AtomImpl a = AtomImpl (Am a) (Expr (Am a))
  deriving (Eq, Ord, Show)

-- [[ ImplImpl a b c ]] = (a ==> b) ==> c
data ImplImpl a = ImplImpl !(Expr (Am a)) !(Expr (Am a)) !(Expr (Am a))
  deriving (Eq, Ord, Show)
