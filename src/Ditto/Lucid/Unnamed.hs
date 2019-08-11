{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ditto.Lucid.Unnamed where

import Data.Foldable (traverse_, fold)
import Ditto.Backend
import Ditto.Core
import Ditto.Generalized.Unnamed as G
import Ditto.Lucid
import Ditto.Types
import Lucid
import Web.PathPieces
import qualified Data.Text as T

foldTraverse_ :: (Foldable t, Applicative f, Monoid (f b)) => (a -> t (f b)) -> t a -> f ()
foldTraverse_ f = traverse_ (fold . f)

inputText
  :: (Monad m, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> text
  -> Form m input err (HtmlT f ()) text
inputText getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "text", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputPassword
  :: (Monad m, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> text
  -> Form m input err (HtmlT f ()) text
inputPassword getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "password", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputSubmit
  :: (Monad m, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> text
  -> Form m input err (HtmlT f ()) (Maybe text)
inputSubmit getInput initialValue = G.inputMaybe getInput inputField (Just initialValue)
  where
  inputField i a = input_ [type_ "submit", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputReset
  :: (Monad m, FormError input err, PathPiece text, Applicative f)
  => text
  -> Form m input err (HtmlT f ()) ()
inputReset lbl = G.inputNoData inputField
  where
  inputField i = input_ [type_ "submit", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece lbl)]

inputHidden
  :: (Monad m, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> text
  -> Form m input err (HtmlT f ()) text
inputHidden getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "hidden", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputButton
  :: (Monad m, FormError input err, PathPiece text, Applicative f)
  => text
  -> Form m input err (HtmlT f ()) ()
inputButton lbl = G.inputNoData inputField
  where
  inputField i = input_ [type_ "button", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece lbl)]

textarea
  :: (Monad m, FormError input err, ToHtml text, Monad f)
  => (input -> Either err text)
  -> Int -- ^ cols
  -> Int -- ^ rows
  -> text -- ^ initial text
  -> Form m input err (HtmlT f ()) text
textarea getInput cols rows initialValue = G.input getInput textareaView initialValue
  where
  textareaView i txt =
    textarea_
      [ rows_ (toPathPiece rows)
      , cols_ (toPathPiece cols)
      , id_ (encodeFormId i)
      , name_ (encodeFormId i)
      ] $
      toHtml txt

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be \"\" and the file contents will be empty as well.
inputFile
  :: (Monad m, FormError input err, FormInput input, Applicative f)
  => Form m input err (HtmlT f ()) (FileType input)
inputFile = G.inputFile fileView
  where
  fileView i = input_ [type_ "file", id_ (encodeFormId i), name_ (encodeFormId i)]

-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit
  :: (Monad m, FormError input err, PathPiece text, ToHtml children, Monad f)
  => (input -> Either err text)
  -> text
  -> children
  -> Form m input err (HtmlT f ()) (Maybe text)
buttonSubmit getInput text c = G.inputMaybe getInput inputField (Just text)
  where
  inputField i a = button_ [type_ "submit", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)] $ toHtml c

-- | create a  @\<button type=\"reset\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
buttonReset
  :: (Monad m, FormError input err, ToHtml children, Monad f)
  => children
  -> Form m input err (HtmlT f ()) ()
buttonReset c = G.inputNoData inputField 
  where
  inputField i = button_ [type_ "reset", id_ (encodeFormId i), name_ (encodeFormId i)] $ toHtml c

-- | create a  @\<button type=\"button\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
button
  :: (Monad m, FormError input err, ToHtml children, Monad f)
  => children
  -> Form m input err (HtmlT f ()) ()
button c = G.inputNoData inputField
  where
  inputField i = button_ [type_ "button", id_ (encodeFormId i), name_ (encodeFormId i)] $ toHtml c

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
label
  :: (Monad m, Monad f)
  => HtmlT f ()
  -> Form m input err (HtmlT f ()) ()
label c = G.label mkLabel
  where
  mkLabel i = label_ [for_ (encodeFormId i)] c

arbitraryHtml :: Monad m => view -> Form m input err view ()
arbitraryHtml wrap =
  Form (successDecode ()) () $ do
    id' <- getFormId
    pure
      ( View (const $ wrap)
      , pure
        ( Ok $ Proved
          { pos = unitRange id'
          , unProved = ()
          }
        )
      )

inputInt
  :: (Monad m, FormError input err, Applicative f)
  => (input -> Either err Int)
  -> Int
  -> Form m input err (HtmlT f ()) Int
inputInt getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a =
    input_
      [ type_ "number"
      , id_ (encodeFormId i)
      , name_ (encodeFormId i)
      , value_ (toPathPiece a)
      ]

inputDouble
  :: (Monad m, FormError input err, Applicative f)
  => (input -> Either err Double)
  -> Double
  -> Form m input err (HtmlT f ()) Double
inputDouble getInput initialValue = G.input getInput inputField initialValue
  where
  inputField i a = input_ [type_ "number", step_ "any", id_ (encodeFormId i), name_ (encodeFormId i), value_ (T.pack $ show a)]

-- | Create a single @\<input type=\"checkbox\"\>@ element
--
-- returns a 'Bool' indicating if it was checked or not.
--
-- see also 'inputCheckboxes'
inputCheckbox
  :: forall err input m f. (Monad m, FormError input err, Applicative f)
  => Bool -- ^ initially checked
  -> Form m input err (HtmlT f ()) Bool
inputCheckbox initiallyChecked =
  Form (successDecode True) initiallyChecked $ do
    i <- getFormId
    v <- getFormInput' i
    case v of
      Default -> mkCheckbox i initiallyChecked
      Missing -> mkCheckbox i False -- checkboxes only appear in the submitted data when checked
      Found _ -> mkCheckbox i True
  where
  mkCheckbox i checked =
    let checkbox =
          input_ $
            (if checked then (:) checked_ else id)
              [type_ "checkbox", id_ (encodeFormId i), name_ (encodeFormId i), value_ (encodeFormId i)]
     in pure
          ( View $ const $ checkbox
          , pure $
            Ok
              ( Proved
                { pos = unitRange i
                , unProved = if checked then True else False
                }
              )
          )

-- | Create a group of @\<input type=\"checkbox\"\>@ elements
--
inputCheckboxes
  :: (Functor m, Monad m, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => [(a, Html ())] -- ^ value, label, initially checked
  -> (input -> Either err [a])
  -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
  -> Form m input err (HtmlT f ()) [a]
inputCheckboxes choices fromInput isChecked =
  G.inputMulti choices fromInput mkCheckboxes isChecked
  where
  mkCheckboxes nm choices' = foldTraverse_ (mkCheckbox nm) choices'
  mkCheckbox nm (Choice i lbl checked val) =
    [ input_ $
        ( (if checked then (checked_ :) else id)
          [type_ "checkbox", id_ (encodeFormId i), name_ (encodeFormId nm), value_ (toPathPiece val)]
        )
    , label_ [for_ (encodeFormId i)] $ toHtml lbl
    ]

-- | Create a group of @\<input type=\"radio\"\>@ elements
inputRadio
  :: (Functor m, Monad m, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => [(a, Html ())] -- ^ value, label, initially checked
  -> (input -> Either err a)
  -> (a -> Bool) -- ^ isDefault
  -> Form m input err (HtmlT f ()) a
inputRadio choices fromInput isDefault =
  G.inputChoice isDefault choices fromInput mkRadios
  where
  mkRadios nm choices' = foldTraverse_ (mkRadio nm) choices'
  mkRadio nm (Choice i lbl checked val) =
    [ input_ $
        (if checked then (checked_ :) else id)
          [type_ "radio", id_ (encodeFormId i), name_ (encodeFormId nm), value_ (toPathPiece val)]
    , label_ [for_ (encodeFormId i)] $ toHtml lbl
    , br_ []
    ]

-- | create @\<select\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- see also: 'selectMultiple'
select
  :: (Functor m, Monad m, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => [(a, Html ())] -- ^ value, label
  -> (input -> Either err a)
  -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
  -> Form m input err (HtmlT f ()) a
select choices fromInput isDefault =
  G.inputChoice isDefault choices fromInput mkSelect
  where
  mkSelect :: (ToHtml lbl, Monad f, PathPiece a) => FormId -> [Choice lbl a] -> HtmlT f ()
  mkSelect nm choices' =
    select_ [name_ (encodeFormId nm)] $
      traverse_ mkOption choices'
  mkOption :: (ToHtml lbl, Monad f, PathPiece a) => Choice lbl a -> HtmlT f ()
  mkOption (Choice _ lbl selected val) =
    option_
      ( (if selected then ((:) (selected_ "selected")) else id)
        [value_ (toPathPiece val)]
      )
      (toHtml lbl)

-- | create @\<select multiple=\"multiple\"\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- This creates a @\<select\>@ element which allows more than one item to be selected.
selectMultiple
  :: (Functor m, Monad m, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => [(a, Html ())] -- ^ value, label, initially checked
  -> (input -> Either err [a])
  -> (a -> Bool) -- ^ isSelected initially
  -> Form m input err (HtmlT f ()) [a]
selectMultiple choices fromInput isSelected =
  G.inputMulti choices fromInput mkSelect isSelected
  where
  mkSelect :: (ToHtml lbl, Monad f, PathPiece a) => FormId -> [Choice lbl a] -> HtmlT f ()
  mkSelect nm choices' =
    select_ [name_ (encodeFormId nm), multiple_ "multiple"] $
      traverse_ mkOption choices'
  mkOption :: (ToHtml lbl, Monad f, PathPiece a) => Choice lbl a -> HtmlT f ()
  mkOption (Choice _ lbl selected val) =
    option_
      ( (if selected then ((:) (selected_ "selected")) else id)
        [value_ (toPathPiece val)]
      )
      (toHtml lbl)

