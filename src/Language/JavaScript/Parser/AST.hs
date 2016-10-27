{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}

module Language.JavaScript.Parser.AST
    ( JSExpression (..)
    , JSAnnot (..)
    , JSBinOp (..)
    , JSUnaryOp (..)
    , JSSemi (..)
    , JSAssignOp (..)
    , JSTryCatch (..)
    , JSTryFinally (..)
    , JSStatement (..)
    , JSBlock (..)
    , JSSwitchParts (..)
    , JSAST (..)
    , JSObjectProperty (..)
    , JSPropertyName (..)
    , JSObjectPropertyList
    , JSAccessor (..)
    , JSIdent (..)
    , JSVarInitializer (..)
    , JSArrayElement (..)
    , JSCommaList (..)
    , JSCommaTrailingList (..)

    , showStripped
    ) where

import Data.Data
import Data.List
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

data JSAnnot
    = JSAnnot !TokenPosn ![CommentAnnotation] -- ^Annotation: position and comment/whitespace information
    | JSNoAnnot -- ^No annotation
    deriving (Data, Eq, Show, Typeable, Read)


data JSAST
    = JSAstProgram ![JSStatement] !JSAnnot -- ^source elements, tailing whitespace
    | JSAstStatement !JSStatement !JSAnnot
    | JSAstExpression !JSExpression !JSAnnot
    | JSAstLiteral !JSExpression !JSAnnot
    deriving (Data, Eq, Show, Typeable, Read)

data JSStatement
    = JSStatementBlock !JSAnnot ![JSStatement] !JSAnnot !JSSemi     -- ^lbrace, stmts, rbrace, autosemi
    | JSBreak !JSAnnot !JSIdent !JSSemi        -- ^break,optional identifier, autosemi
    | JSConstant !JSAnnot !(JSCommaList JSExpression) !JSSemi -- ^const, decl, autosemi
    | JSContinue !JSAnnot !JSIdent !JSSemi     -- ^continue, optional identifier,autosemi
    | JSDoWhile !JSAnnot !JSStatement !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSSemi -- ^do,stmt,while,lb,expr,rb,autosemi
    | JSFor !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement -- ^for,lb,expr,semi,expr,semi,expr,rb.stmt
    | JSForIn !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement -- ^for,lb,expr,in,expr,rb,stmt
    | JSForVar !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement -- ^for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    | JSForVarIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement -- ^for,lb,var,vardecl,in,expr,rb,stmt
    | JSFunction !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSIdent) !JSAnnot !JSBlock !JSSemi  -- ^fn,name, lb,parameter list,rb,block,autosemi
    | JSIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement -- ^if,(,expr,),stmt
    | JSIfElse !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSAnnot !JSStatement -- ^if,(,expr,),stmt,else,rest
    | JSLabelled !JSIdent !JSAnnot !JSStatement -- ^identifier,colon,stmt
    | JSEmptyStatement !JSAnnot
    | JSExpressionStatement !JSExpression !JSSemi
    | JSAssignStatement !JSExpression !JSAssignOp !JSExpression !JSSemi -- ^lhs, assignop, rhs, autosemi
    | JSMethodCall !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSSemi
    | JSReturn !JSAnnot !(Maybe JSExpression) !JSSemi -- ^optional expression,autosemi
    | JSSwitch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSAnnot ![JSSwitchParts] !JSAnnot !JSSemi -- ^switch,lb,expr,rb,caseblock,autosemi
    | JSThrow !JSAnnot !JSExpression !JSSemi -- ^throw val autosemi
    | JSTry !JSAnnot !JSBlock ![JSTryCatch] !JSTryFinally -- ^try,block,catches,finally
    | JSVariable !JSAnnot !(JSCommaList JSExpression) !JSSemi -- ^var|const, decl, autosemi
    | JSWhile !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement -- ^while,lb,expr,rb,stmt
    | JSWith !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSSemi -- ^with,lb,expr,rb,stmt list
    deriving (Data, Eq, Show, Typeable, Read)

data JSExpression
    -- | Terminals
    = JSIdentifier !JSAnnot !String
    | JSDecimal !JSAnnot !String
    | JSLiteral !JSAnnot !String
    | JSHexInteger !JSAnnot !String
    | JSOctal !JSAnnot !String
    | JSStringLiteral !JSAnnot !String
    | JSRegEx !JSAnnot !String

    -- | Non Terminals
    | JSArrayLiteral !JSAnnot ![JSArrayElement] !JSAnnot -- ^lb, contents, rb
    | JSAssignExpression !JSExpression !JSAssignOp !JSExpression -- ^lhs, assignop, rhs
    | JSCallExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot  -- ^expr, bl, args, rb
    | JSCallExpressionDot !JSExpression !JSAnnot !JSExpression  -- ^expr, dot, expr
    | JSCallExpressionSquare !JSExpression !JSAnnot !JSExpression !JSAnnot  -- ^expr, [, expr, ]
    | JSCommaExpression !JSExpression !JSAnnot !JSExpression          -- ^expression components
    | JSExpressionBinary !JSExpression !JSBinOp !JSExpression -- ^lhs, op, rhs
    | JSExpressionParen !JSAnnot !JSExpression !JSAnnot -- ^lb,expression,rb
    | JSExpressionPostfix !JSExpression !JSUnaryOp -- ^expression, operator
    | JSExpressionTernary !JSExpression !JSAnnot !JSExpression !JSAnnot !JSExpression -- ^cond, ?, trueval, :, falseval
    | JSFunctionExpression !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSIdent) !JSAnnot !JSBlock -- ^fn,name,lb, parameter list,rb,block`
    | JSMemberDot !JSExpression !JSAnnot !JSExpression -- ^firstpart, dot, name
    | JSMemberExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot -- expr, lb, args, rb
    | JSMemberNew !JSAnnot !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot -- ^new, name, lb, args, rb
    | JSMemberSquare !JSExpression !JSAnnot !JSExpression !JSAnnot -- ^firstpart, lb, expr, rb
    | JSNewExpression !JSAnnot !JSExpression -- ^new, expr
    | JSObjectLiteral !JSAnnot !JSObjectPropertyList !JSAnnot -- ^lbrace contents rbrace
    | JSUnaryExpression !JSUnaryOp !JSExpression
    | JSVarInitExpression !JSExpression !JSVarInitializer -- ^identifier, initializer
    deriving (Data, Eq, Show, Typeable, Read)

data JSBinOp
    = JSBinOpAnd !JSAnnot
    | JSBinOpBitAnd !JSAnnot
    | JSBinOpBitOr !JSAnnot
    | JSBinOpBitXor !JSAnnot
    | JSBinOpDivide !JSAnnot
    | JSBinOpEq !JSAnnot
    | JSBinOpGe !JSAnnot
    | JSBinOpGt !JSAnnot
    | JSBinOpIn !JSAnnot
    | JSBinOpInstanceOf !JSAnnot
    | JSBinOpLe !JSAnnot
    | JSBinOpLsh !JSAnnot
    | JSBinOpLt !JSAnnot
    | JSBinOpMinus !JSAnnot
    | JSBinOpMod !JSAnnot
    | JSBinOpNeq !JSAnnot
    | JSBinOpOr !JSAnnot
    | JSBinOpPlus !JSAnnot
    | JSBinOpRsh !JSAnnot
    | JSBinOpStrictEq !JSAnnot
    | JSBinOpStrictNeq !JSAnnot
    | JSBinOpTimes !JSAnnot
    | JSBinOpUrsh !JSAnnot
    deriving (Data, Eq, Show, Typeable, Read)

data JSUnaryOp
    = JSUnaryOpDecr !JSAnnot
    | JSUnaryOpDelete !JSAnnot
    | JSUnaryOpIncr !JSAnnot
    | JSUnaryOpMinus !JSAnnot
    | JSUnaryOpNot !JSAnnot
    | JSUnaryOpPlus !JSAnnot
    | JSUnaryOpTilde !JSAnnot
    | JSUnaryOpTypeof !JSAnnot
    | JSUnaryOpVoid !JSAnnot
    deriving (Data, Eq, Show, Typeable, Read)

data JSSemi
    = JSSemi !JSAnnot
    | JSSemiAuto
    deriving (Data, Eq, Show, Typeable, Read)

data JSAssignOp
    = JSAssign !JSAnnot
    | JSTimesAssign !JSAnnot
    | JSDivideAssign !JSAnnot
    | JSModAssign !JSAnnot
    | JSPlusAssign !JSAnnot
    | JSMinusAssign !JSAnnot
    | JSLshAssign !JSAnnot
    | JSRshAssign !JSAnnot
    | JSUrshAssign !JSAnnot
    | JSBwAndAssign !JSAnnot
    | JSBwXorAssign !JSAnnot
    | JSBwOrAssign !JSAnnot
    deriving (Data, Eq, Show, Typeable, Read)

data JSTryCatch
    = JSCatch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSBlock -- ^catch,lb,ident,rb,block
    | JSCatchIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSExpression !JSAnnot !JSBlock -- ^catch,lb,ident,if,expr,rb,block
    deriving (Data, Eq, Show, Typeable, Read)

data JSTryFinally
    = JSFinally !JSAnnot !JSBlock -- ^finally,block
    | JSNoFinally
    deriving (Data, Eq, Show, Typeable, Read)

data JSBlock
    = JSBlock !JSAnnot ![JSStatement] !JSAnnot -- ^lbrace, stmts, rbrace
    deriving (Data, Eq, Show, Typeable, Read)

data JSSwitchParts
    = JSCase !JSAnnot !JSExpression !JSAnnot ![JSStatement]    -- ^expr,colon,stmtlist
    | JSDefault !JSAnnot !JSAnnot ![JSStatement] -- ^colon,stmtlist
    deriving (Data, Eq, Show, Typeable, Read)

data JSVarInitializer
    = JSVarInit !JSAnnot !JSExpression -- ^ assignop, initializer
    | JSVarInitNone
    deriving (Data, Eq, Show, Typeable, Read)

data JSObjectProperty
    = JSPropertyAccessor !JSAccessor !JSPropertyName !JSAnnot ![JSExpression] !JSAnnot !JSBlock -- ^(get|set), name, lb, params, rb, block
    | JSPropertyNameandValue !JSPropertyName !JSAnnot ![JSExpression] -- ^name, colon, value
    deriving (Data, Eq, Show, Typeable, Read)

data JSPropertyName
    = JSPropertyIdent !JSAnnot !String
    | JSPropertyString !JSAnnot !String
    | JSPropertyNumber !JSAnnot !String
    deriving (Data, Eq, Show, Typeable, Read)

type JSObjectPropertyList = JSCommaTrailingList JSObjectProperty

-- | Accessors for JSObjectProperty is either 'get' or 'set'.
data JSAccessor
    = JSAccessorGet !JSAnnot
    | JSAccessorSet !JSAnnot
    deriving (Data, Eq, Show, Typeable, Read)

data JSIdent
    = JSIdentName !JSAnnot !String
    | JSIdentNone
    deriving (Data, Eq, Show, Typeable, Read)

data JSArrayElement
    = JSArrayElement !JSExpression
    | JSArrayComma !JSAnnot
    deriving (Data, Eq, Show, Typeable, Read)

data JSCommaList a
    = JSLCons !(JSCommaList a) !JSAnnot !a -- ^head, comma, a
    | JSLOne !a -- ^ single element (no comma)
    | JSLNil
    deriving (Data, Eq, Show, Typeable, Read)

data JSCommaTrailingList a
    = JSCTLComma !(JSCommaList a) !JSAnnot -- ^list, trailing comma
    | JSCTLNone !(JSCommaList a) -- ^list
    deriving (Data, Eq, Show, Typeable, Read)

-- -----------------------------------------------------------------------------
-- | Show the AST elements stipped of their JSAnnot data.

-- Strip out the location info
showStripped :: JSAST -> String
showStripped (JSAstProgram xs _) = "JSAstProgram " ++ ss xs ++ " JSNoAnnot"
showStripped (JSAstStatement s _) = "JSAstStatement " ++ ss s ++ " JSNoAnnot"
showStripped (JSAstExpression e _) = "JSAstExpression " ++ ss e ++ " JSNoAnnot"
showStripped (JSAstLiteral s _)  = "JSAstLiteral " ++ ss s ++ " JSNoAnnot"


class ShowStripped a where
    ss :: a -> String


instance ShowStripped JSStatement where
    ss (JSStatementBlock _ xs _ s) = "JSStatementBlock " ++ "JSNoAnnot (" ++ ss xs ++ ") JSNoAnnot " ++ ss s
    ss (JSBreak _ x s) = "JSBreak " ++ "JSNoAnnot (" ++ ss x ++ ") (" ++ ss s ++ ")"
    ss (JSContinue _ x s) = "JSContinue" ++ " JSNoAnnot (" ++ ss x ++ ") (" ++ ss s ++ ")"
    ss (JSConstant _ xs _as) = "JSConstant "  ++ "JSNoAnnot (" ++ ss xs ++ ")"
    ss (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile JSNoAnnot (" ++ ss x1 ++ ") JSNoAnnot JSNoAnnot (" ++ ss x2 ++ ") JSNoAnnot (" ++ ss x3 ++ ")"
    ss (JSFor _ _ x1s _ x2s _ x3s _ x4) = "JSFor JSNoAnnot JSNoAnnot (" ++ ss x1s ++ ") JSNoAnnot (" ++ ss x2s ++ ") JSNoAnnot (" ++ ss x3s ++ ") JSNoAnnot (" ++ ss x4 ++ ")"
    ss (JSForIn _ _ x1s op x2s _ st) = "JSForIn JSNoAnnot JSNoAnnot (" ++ ss x1s ++ ") (" ++ ss op ++ ") (" ++ ss x2s ++ ") JSNoAnnot (" ++ ss st ++ ")"
    ss (JSForVar _ _ _ x1s _ x2s _ x3s _ st) = "JSForVar JSNoAnnot JSNoAnnot JSNoAnnot (" ++ ss x1s ++ ") JSNoAnnot (" ++ ss x2s ++ ") JSNoAnnot (" ++ ss x3s ++ ") JSNoAnnot (" ++ ss st ++ ")"
    ss (JSForVarIn _ _ _ x1 _ x2 _ x3) = "JSForVarIn JSNoAnnot JSNoAnnot JSNoAnnot " ++ "(" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2 ++ ") JSNoAnnot (" ++ ss x3 ++ ")"
    ss (JSFunction _ n _ pl _ x3 s) = "JSFunction JSNoAnnot (" ++ ss n ++ ") JSNoAnnot (" ++ ss pl ++ ") JSNoAnnot (" ++ ss x3 ++ ") (" ++ ss s ++ ")"
    ss (JSIf _ _ x1 _rb x2) = "JSIf JSNoAnnot JSNoAnnot (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2 ++ ")"
    ss (JSIfElse _ _lb x1 _rb x2 _e x3) = "JSIfElse JSNoAnnot JSNoAnnot (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2 ++ ") JSNoAnnot (" ++ ss x3 ++ ")"
    ss (JSLabelled x1 _c x2) = "JSLabelled (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2 ++ ")"
    ss (JSEmptyStatement _) = "JSEmptyStatement JSNoAnnot"
    ss (JSExpressionStatement l s) = "JSExpressionStatement (" ++ ss l ++ ") (" ++ ss s ++ ")"
    ss (JSAssignStatement lhs op rhs s) ="JSAssignStatement (" ++ ss lhs ++ ") (" ++ ss op ++ ") (" ++ ss rhs ++ ") (" ++ ss s ++ ")"
    ss (JSMethodCall e _ a _ s) = "JSMethodCall (" ++ ss e ++ ") JSNoAnnot (" ++ ss a ++ ") JSNoAnnot (" ++ ss s ++ ")"
    ss (JSReturn _ (Just me) s) = "JSReturn JSNoAnnot " ++ "(Just (" ++ ss me ++ ")) (" ++ ss s ++ ")"
    ss (JSReturn _ Nothing s) = "JSReturn JSNoAnnot Nothing (" ++ ss s ++ ")"
    ss (JSSwitch _ _ x _ _ x2 _ s) = "JSSwitch JSNoAnnot JSNoAnnot (" ++ ss x ++ ") JSNoAnnot JSNoAnnot (" ++ ss x2 ++ ") JSNoAnnot (" ++ ss s ++ ")"
    ss (JSThrow _ x s) = "JSThrow JSNoAnnot (" ++ ss x ++ ") (" ++ ss s ++ ")"
    ss (JSTry _ xt1 xtc xtf) = "JSTry JSNoAnnot (" ++ ss xt1 ++ ") (" ++ ss xtc ++ ") (" ++ ss xtf ++ ")"
    ss (JSVariable _ xs s) = "JSVariable JSNoAnnot (" ++ ss xs ++ ") (" ++ ss s ++ ")"
    ss (JSWhile _ _ x1 _ x2) = "JSWhile JSNoAnnot JSNoAnnot (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"
    ss (JSWith _ _ x1 _ x s) = "JSWith JSNoAnnot JSNoAnnot (" ++ ss x1 ++ ") (" ++ ss x ++ ") (" ++ ss s ++ ")"

instance ShowStripped JSExpression where
    ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral JSNoAnnot " ++ ss xs ++ " JSNoAnnot"
    ss (JSAssignExpression lhs op rhs) = "JSAssignExpression (" ++ ss lhs ++ ") (" ++ ss op ++ ") (" ++ ss rhs ++ ")"
    ss (JSCallExpression ex _ xs _) = "JSCallExpression (" ++ ss ex ++ ") JSNoAnnot (" ++ ss xs ++ ") JSNoAnnot"
    ss (JSCallExpressionDot ex _os xs) = "JSCallExpressionDot (" ++ ss ex ++ ") JSNoAnnot (" ++ ss xs ++ ")"
    ss (JSCallExpressionSquare ex _ xs _) = "JSCallExpressionSquare (" ++ ss ex ++ ") JSNoAnnot (" ++ ss xs ++ ") JSNoAnnot"
    ss (JSDecimal _ s) = "JSDecimal JSNoAnnot " ++ doubleQuote s
    ss (JSCommaExpression l _ r) = "JSCommaExpression (" ++ ss l ++ ") JSNoAnnot (" ++ ss r ++ ")"
    ss (JSExpressionBinary x1 op x2) = "JSExpressionBinary (" ++ ss x1 ++ ") (" ++ ss op ++ ") (" ++ ss x2 ++ ")"
    ss (JSExpressionParen _lp x _rp) = "JSExpressionParen JSNoAnnot (" ++ ss x ++ ") JSNoAnnot"
    ss (JSExpressionPostfix xs op) = "JSExpressionPostfix (" ++ ss xs ++ ") (" ++ ss op ++ ")"
    ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2 ++ ") JSNoAnnot (" ++ ss x3 ++ ")"
    ss (JSFunctionExpression _ n _lb pl _rb x3) = "JSFunctionExpression JSNoAnnot (" ++ ss n ++ ") JSNoAnnot (" ++ ss pl ++ ") JSNoAnnot (" ++ ss x3 ++ ")"
    ss (JSHexInteger _ s) = "JSHexInteger JSNoAnnot " ++ doubleQuote s
    ss (JSOctal _ s) = "JSOctal JSNoAnnot " ++ doubleQuote s
    ss (JSIdentifier _ s) = "JSIdentifier JSNoAnnot " ++ doubleQuote s
    ss (JSLiteral _ s) = "JSLiteral JSNoAnnot " ++ doubleQuote s
    ss (JSMemberDot x1s _d x2 ) = "JSMemberDot (" ++ ss x1s ++ ") JSNoAnnot (" ++ ss x2 ++ ")"
    ss (JSMemberExpression e _ a _) = "JSMemberExpression (" ++ ss e ++ ") JSNoAnnot (" ++ ss a ++ ") JSNoAnnot"
    ss (JSMemberNew _ n _ s _) = "JSMemberNew JSNoAnnot (" ++ ss n ++ ") JSNoAnnot (" ++ ss s ++ ") JSNoAnnot"
    ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare (" ++ ss x1s ++ ") JSNoAnnot (" ++ ss x2 ++ ") JSNoAnnot"
    ss (JSNewExpression _n e) = "JSNewExpression JSNoAnnot (" ++ ss e ++ ")"
    ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral JSNoAnnot (" ++ ss xs ++ ") JSNoAnnot"
    ss (JSRegEx _ s) = "JSRegEx JSNoAnnot " ++ doubleQuote s
    ss (JSStringLiteral _ s) = "JSStringLiteral JSNoAnnot " ++ doubleQuote s
    ss (JSUnaryExpression op x) = "JSUnaryExpression (" ++ ss op ++ ") (" ++ ss x ++ ")"
    ss (JSVarInitExpression x1 x2) = "JSVarInitExpression (" ++ ss x1 ++ ") (" ++ ss x2 ++ ")"

instance ShowStripped JSTryCatch where
    ss (JSCatch _ _ x1 _ x3) = "JSCatch JSNoAnnot JSNoAnnot (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x3 ++ ")"
    ss (JSCatchIf _ _ x1 _ ex _ x3) = "JSCatch JSNoAnnot JSNoAnnot (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss ex ++ ") JSNoAnnot (" ++ ss x3 ++ ")"

instance ShowStripped JSTryFinally where
    ss (JSFinally _ x) = "JSFinally JSNoAnnot (" ++ ss x ++ ")"
    ss JSNoFinally = "JSNoFinally"

instance ShowStripped JSIdent where
    ss (JSIdentName _ s) = "JSIdentName JSNoAnnot " ++ doubleQuote s
    ss JSIdentNone = "JSIdentNone"

instance ShowStripped JSObjectProperty where
    ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2s ++ ")"
    ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " ++ ss s ++ " (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2s ++ ") JSNoAnnot (" ++ ss x3 ++ ")"

instance ShowStripped JSPropertyName where
    ss (JSPropertyIdent _ s) = "JSPropertyIdent JSNoAnnot " ++ doubleQuote s
    ss (JSPropertyString _ s) = "JSPropertyString JSNoAnnot " ++ doubleQuote s
    ss (JSPropertyNumber _ s) = "JSPropertyNumber JSNoAnnot " ++ doubleQuote s

instance ShowStripped JSAccessor where
    ss (JSAccessorGet _) = "JSAccessorGet JSNoAnnot"

instance ShowStripped JSBlock where
    ss (JSBlock _ xs _) = "JSBlock " ++ "JSNoAnnot " ++ ss xs ++ " JSNoAnnot"

instance ShowStripped JSSwitchParts where
    ss (JSCase _ x1 _c x2s) = "JSCase JSNoAnnot (" ++ ss x1 ++ ") JSNoAnnot (" ++ ss x2s ++ ")"
    ss (JSDefault _ _c xs) = "JSDefault JSNoAnnot JSNoAnnot (" ++ ss xs ++ ")"

instance ShowStripped JSBinOp where
    ss (JSBinOpAnd _) = "JSBinOpAnd JSNoAnnot"
    ss (JSBinOpBitAnd _) = "JSBinOpBitAnd JSNoAnnot"
    ss (JSBinOpBitOr _) = "JSBinOpBitOr JSNoAnnot"
    ss (JSBinOpBitXor _) = "JSBinOpBitXor JSNoAnnot"
    ss (JSBinOpDivide _) = "JSBinOpDivide JSNoAnnot"
    ss (JSBinOpEq _) = "JSBinOpEq JSNoAnnot"
    ss (JSBinOpGe _) = "JSBinOpGe JSNoAnnot"
    ss (JSBinOpGt _) = "JSBinOpGt JSNoAnnot"
    ss (JSBinOpIn _) = "JSBinOpIn JSNoAnnot"
    ss (JSBinOpInstanceOf _) = "JSBinOpInstanceOf JSNoAnnot"
    ss (JSBinOpLe _) = "JSBinOpLe JSNoAnnot"
    ss (JSBinOpLsh _) = "JSBinOpLsh JSNoAnnot"
    ss (JSBinOpLt _) = "JSBinOpLt JSNoAnnot"
    ss (JSBinOpMinus _) = "JSBinOpMinus JSNoAnnot"
    ss (JSBinOpMod _) = "JSBinOpMod JSNoAnnot"
    ss (JSBinOpNeq _) = "JSBinOpNeq JSNoAnnot"
    ss (JSBinOpOr _) = "JSBinOpOr JSNoAnnot"
    ss (JSBinOpPlus _) = "JSBinOpPlus JSNoAnnot"
    ss (JSBinOpRsh _) = "JSBinOpRsh JSNoAnnot"
    ss (JSBinOpStrictEq _) = "JSBinOpStrictEq JSNoAnnot"
    ss (JSBinOpStrictNeq _) = "JSBinOpStrictNeq JSNoAnnot"
    ss (JSBinOpTimes _) = "JSBinOpTimes JSNoAnnot"
    ss (JSBinOpUrsh _) = "JSBinOpUrsh JSNoAnnot"

instance ShowStripped JSUnaryOp where
    ss (JSUnaryOpDecr _) = "JSUnaryOpDecr JSNoAnnot"
    ss (JSUnaryOpDelete _) = "JSUnaryOpDelete JSNoAnnot"
    ss (JSUnaryOpIncr _) = "JSUnaryOpIncr JSNoAnnot"
    ss (JSUnaryOpMinus _) = "JSUnaryOpMinus JSNoAnnot"
    ss (JSUnaryOpNot _) = "JSUnaryOpNot JSNoAnnot"
    ss (JSUnaryOpPlus _) = "JSUnaryOpPlus JSNoAnnot"
    ss (JSUnaryOpTilde _) = "JSUnaryOpTilde JSNoAnnot"
    ss (JSUnaryOpTypeof _) = "JSUnaryOpTypeof JSNoAnnot"
    ss (JSUnaryOpVoid _) = "JSUnaryOpVoid JSNoAnnot"

instance ShowStripped JSAssignOp where
    ss (JSAssign _) = "JSAssign JSNoAnnot"
    ss (JSTimesAssign _) = "JSTimesAssign JSNoAnnot"
    ss (JSDivideAssign _) = "JSDivideAssign JSNoAnnot"
    ss (JSModAssign _) = "JSModAssign JSNoAnnot"
    ss (JSPlusAssign _) = "JSPlusAssign JSNoAnnot"
    ss (JSMinusAssign _) = "JSMinusAssign JSNoAnnot"
    ss (JSLshAssign _) = "JSLshAssign JSNoAnnot"
    ss (JSRshAssign _) = "JSRshAssign JSNoAnnot"
    ss (JSUrshAssign _) = "JSUrshAssign JSNoAnnot"
    ss (JSBwAndAssign _) = "JSBwAndAssign JSNoAnnot"
    ss (JSBwXorAssign _) = "JSBwXorAssign JSNoAnnot"
    ss (JSBwOrAssign _) = "JSBwOrAssign JSNoAnnot"

instance ShowStripped JSVarInitializer where
    ss (JSVarInit _ n) = "JSVarInit JSNoAnnot (" ++ ss n ++ ")"
    ss JSVarInitNone = "JSVarInitNone"

instance ShowStripped JSSemi where
    ss (JSSemi _) = "JSSemi JSNoAnnot"
    ss JSSemiAuto = "JSSemiAuto"

instance ShowStripped JSArrayElement where
    ss (JSArrayElement e) = "JSArrayElement (" ++ ss e ++ ")"
    ss (JSArrayComma _) = "JSArrayComma JSNoAnnot"

instance ShowStripped a => ShowStripped (JSCommaList a) where
    ss (JSLCons xs _ x) = "JSLCons (" ++ ss xs ++ ") JSNoAnnot (" ++ ss x ++ ")"
    ss (JSLOne x) = "JSLOne (" ++ ss x ++ ")"
    ss JSLNil = "JSLNil"

instance ShowStripped a => ShowStripped (JSCommaTrailingList a) where
    ss (JSCTLComma xs _) = "JSCTLComma (" ++ ss xs ++ ") JSNoAnnot" 
    ss (JSCTLNone xs)    = "JSCTLNone (" ++ ss xs ++ ")"

instance ShowStripped a => ShowStripped [a] where
    ss xs = "[" ++ commaJoin (map ss xs) ++ "]"

-- -----------------------------------------------------------------------------
-- Helpers.

commaJoin :: [String] -> String
commaJoin s = intercalate "," $ filter (not . null) s

fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons l _ i) = fromCommaList l ++ [i]
fromCommaList (JSLOne i)      = [i]
fromCommaList JSLNil = []

singleQuote :: String -> String
singleQuote s = '\'' : (s ++ "'")

doubleQuote :: String -> String
doubleQuote s@('\"':_) = show s
doubleQuote s = '\"' : (s ++ "\"")

ssid :: JSIdent -> String
ssid (JSIdentName _ s) = singleQuote s
ssid JSIdentNone = "''"

commaIf :: String -> String
commaIf "" = ""
commaIf xs = ',' : xs
