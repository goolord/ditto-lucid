{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ditto.Lucid.Named where

import Data.Foldable (traverse_, fold)
import Data.List.NonEmpty
import Data.Text (Text)
import Ditto.Backend
import Ditto.Core
import Ditto.Generalized.Named as G
import Ditto.Types
import Lucid
import Web.PathPieces
import qualified Data.Text as T

foldTraverse_ :: (Foldable t, Applicative f, Monoid (f b)) => (a -> t (f b)) -> t a -> f ()
foldTraverse_ f = traverse_ (fold . f)

inputText
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> Text
  -> text
  -> Form m input err (HtmlT f ()) text
inputText getInput name initialValue = G.input name getInput inputField initialValue
  where
  inputField i a = input_ [type_ "text", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputMaybeText
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> Text
  -> Maybe text
  -> Form m input err (HtmlT f ()) (Maybe text)
inputMaybeText getInput name initialValue = G.inputMaybe name getInput inputField initialValue
  where
  inputField i a = let attrs = maybe [] (pure . value_ . toPathPiece) a in 
    input_ $ type_ "text" : id_ (encodeFormId i) : name_ (encodeFormId i) : attrs

inputPassword
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> Text
  -> text
  -> Form m input err (HtmlT f ()) text
inputPassword getInput name initialValue = G.input name getInput inputField initialValue
  where
  inputField i a = input_ [type_ "password", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputSubmit
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> Text
  -> text
  -> Form m input err (HtmlT f ()) (Maybe text)
inputSubmit getInput name initialValue = G.inputMaybe name getInput inputField (Just initialValue)
  where
  inputField i a = input_ [type_ "submit", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputReset
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => Text
  -> text
  -> Form m input err (HtmlT f ()) ()
inputReset name lbl = G.inputNoData name inputField
  where
  inputField i = input_ [type_ "submit", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece lbl)]

inputHidden
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => (input -> Either err text)
  -> Text
  -> text
  -> Form m input err (HtmlT f ()) text
inputHidden getInput name initialValue = G.input name getInput inputField initialValue
  where
  inputField i a = input_ [type_ "hidden", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)]

inputButton
  :: (Environment m input, FormError input err, PathPiece text, Applicative f)
  => Text
  -> text
  -> Form m input err (HtmlT f ()) ()
inputButton name lbl = G.inputNoData name inputField
  where
  inputField i = input_ [type_ "button", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece lbl)]

textarea
  :: (Environment m input, FormError input err, ToHtml text, Monad f)
  => (input -> Either err text)
  -> Int -- ^ cols
  -> Int -- ^ rows
  -> Text
  -> text -- ^ initial text
  -> Form m input err (HtmlT f ()) text
textarea getInput cols rows name initialValue = G.input name getInput textareaView initialValue
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
  :: (Environment m input, FormError input err, FormInput input, Applicative f)
  => Text
  -> Form m input err (HtmlT f ()) (FileType input)
inputFile name = G.inputFile name fileView
  where
  fileView i = input_ [type_ "file", id_ (encodeFormId i), name_ (encodeFormId i)]

-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit
  :: (Environment m input, FormError input err, PathPiece text, ToHtml children, Monad f)
  => (input -> Either err text)
  -> Text
  -> text
  -> children
  -> Form m input err (HtmlT f ()) (Maybe text)
buttonSubmit getInput name text c = G.inputMaybe name getInput inputField (Just text)
  where
  inputField i a = button_ [type_ "submit", id_ (encodeFormId i), name_ (encodeFormId i), value_ (toPathPiece a)] $ toHtml c

-- | create a  @\<button type=\"reset\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
buttonReset
  :: (Environment m input, FormError input err, Monad f)
  => Text
  -> HtmlT f ()
  -> Form m input err (HtmlT f ()) ()
buttonReset name c = G.inputNoData name inputField
  where
  inputField i = button_ [type_ "reset", id_ (encodeFormId i), name_ (encodeFormId i)] c

-- | create a  @\<button type=\"button\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
button
  :: (Environment m input, FormError input err, Monad f)
  => Text
  -> HtmlT f ()
  -> Form m input err (HtmlT f ()) ()
button name c = G.inputNoData name inputField
  where
  inputField i = button_ [type_ "button", id_ (encodeFormId i), name_ (encodeFormId i)] c

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
label
  :: (Environment m input, Monad f)
  => HtmlT f ()
  -> Text
  -> Form m input err (HtmlT f ()) ()
label c name = G.label name mkLabel
  where
  mkLabel i = label_ [for_ (encodeFormId i)] c

arbitraryHtml :: Environment m input => view -> Form m input err view ()
arbitraryHtml = view

inputInt
  :: (Environment m input, FormError input err, Applicative f)
  => (input -> Either err Int)
  -> Text
  -> Int
  -> Form m input err (HtmlT f ()) Int
inputInt getInput name initialValue = G.input name getInput inputField initialValue
  where
  inputField i a =
    input_
      [ type_ "number"
      , id_ (encodeFormId i)
      , name_ (encodeFormId i)
      , value_ (toPathPiece a)
      ]

inputDouble
  :: (Environment m input, FormError input err, Applicative f)
  => (input -> Either err Double)
  -> Text
  -> Double
  -> Form m input err (HtmlT f ()) Double
inputDouble getInput name initialValue = G.input name getInput inputField initialValue
  where
  inputField i a = input_ [type_ "number", step_ "any", id_ (encodeFormId i), name_ (encodeFormId i), value_ (T.pack $ show a)]

-- | Create a single @\<input type=\"checkbox\"\>@ element
--
-- returns a 'Bool' indicating if it was checked or not.
--
-- see also 'inputCheckboxes'
inputCheckbox
  :: forall err input m f. (Environment m input, FormError input err, Applicative f)
  => Bool -- ^ initially checked
  -> Text -- ^
  -> Form m input err (HtmlT f ()) Bool
inputCheckbox initiallyChecked name =
  Form (successDecode True) (pure initiallyChecked) $ do
    i <- getNamedFormId name
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
  :: (Functor m, Environment m input, FormError input err, FormInput input, ToHtml lbl, Monad f, PathPiece a, Eq a)
  => Text
  -> [(a, lbl)] -- ^ value, label, initially checked
  -> (input -> Either err [a])
  -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
  -> Form m input err (HtmlT f ()) [a]
inputCheckboxes name choices fromInput isChecked = G.inputMulti name choices fromInput mkCheckboxes isChecked
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
  :: (Functor m, Environment m input, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => Text
  -> NonEmpty (a, Html ()) -- ^ value, label, initially checked
  -> (input -> Either err a)
  -> (a -> Bool) -- ^ isDefault
  -> Form m input err (HtmlT f ()) a
inputRadio name choices fromInput isDefault =
  G.inputChoice name isDefault choices fromInput mkRadios
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
  :: (Functor m, Environment m input, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => Text
  -> NonEmpty (a, Html ()) -- ^ value, label
  -> (input -> Either err a)
  -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
  -> Form m input err (HtmlT f ()) a
select name choices fromInput isDefault = G.inputChoice name isDefault choices fromInput mkSelect
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
  :: (Functor m, Environment m input, FormError input err, FormInput input, Monad f, PathPiece a, Eq a)
  => Text
  -> [(a, Html ())] -- ^ value, label, initially checked
  -> (input -> Either err [a])
  -> (a -> Bool) -- ^ isSelected initially
  -> Form m input err (HtmlT f ()) [a]
selectMultiple name choices fromInput isSelected = G.inputMulti name choices fromInput mkSelect isSelected
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

