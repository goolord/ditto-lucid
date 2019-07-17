{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ditto.Lucid.Named where

import Data.Foldable (traverse_)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Text (Text)
import Lucid
import Ditto.Backend
import Ditto.Core
import Ditto.Generalized.Named as G
import Ditto.Result (FormId, Result (Ok), unitRange)
import Web.PathPieces
import qualified Data.Text as T
import qualified Text.Read

instance PathPiece FormId where
  toPathPiece fid = T.pack (show fid)
  fromPathPiece fidT = Nothing

inputText
  :: (Monad m, FormError error, PathPiece text, Applicative f)
  => (input -> Either error text)
  -> String
  -> text
  -> Form m input error (HtmlT f ()) () text
inputText getInput name initialValue = G.input getInput inputField initialValue name
  where
    inputField i a = input_ [type_ "text", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)]

inputPassword
  :: (Monad m, FormError error, PathPiece text, Applicative f)
  => (input -> Either error text)
  -> String
  -> text
  -> Form m input error (HtmlT f ()) () text
inputPassword getInput name initialValue = G.input getInput inputField initialValue name
  where
    inputField i a = input_ [type_ "password", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)]

inputSubmit
  :: (Monad m, FormError error, PathPiece text, Applicative f)
  => (input -> Either error text)
  -> String
  -> text
  -> Form m input error (HtmlT f ()) () (Maybe text)
inputSubmit getInput name initialValue = G.inputMaybe getInput inputField initialValue name
  where
    inputField i a = input_ [type_ "submit", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)]

inputReset
  :: (Monad m, FormError error, PathPiece text, Applicative f)
  => String
  -> text
  -> Form m input error (HtmlT f ()) () ()
inputReset name lbl = G.inputNoData inputField lbl name
  where
    inputField i a = input_ [type_ "submit", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)]

inputHidden
  :: (Monad m, FormError error, PathPiece text, Applicative f)
  => (input -> Either error text)
  -> String
  -> text
  -> Form m input error (HtmlT f ()) () text
inputHidden getInput name initialValue = G.input getInput inputField initialValue name
  where
    inputField i a = input_ [type_ "hidden", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)]

inputButton
  :: (Monad m, FormError error, PathPiece text, Applicative f)
  => String
  -> text
  -> Form m input error (HtmlT f ()) () ()
inputButton name label = G.inputNoData inputField label name
  where
    inputField i a = input_ [type_ "button", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)]

textarea
  :: (Monad m, FormError error, ToHtml text, Monad f)
  => (input -> Either error text)
  -> Int -- ^ cols
  -> Int -- ^ rows
  -> String
  -> text -- ^ initial text
  -> Form m input error (HtmlT f ()) () text
textarea getInput cols rows name initialValue = G.input getInput textareaView initialValue name
  where
    textareaView i txt =
      textarea_
        [ rows_ (toPathPiece rows)
        , cols_ (toPathPiece cols)
        , id_ (toPathPiece i)
        , name_ (toPathPiece i)
        ] $
        toHtml txt

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be \"\" and the file contents will be empty as well.
inputFile
  :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input, Applicative f)
  => String
  -> Form m input error (HtmlT f ()) () (FileType input)
inputFile name = G.inputFile fileView name
  where
    fileView i = input_ [type_ "file", id_ (toPathPiece i), name_ (toPathPiece i)]

-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit
  :: (Monad m, FormError error, PathPiece text, ToHtml children, Monad f)
  => (input -> Either error text)
  -> String
  -> text
  -> children
  -> Form m input error (HtmlT f ()) () (Maybe text)
buttonSubmit getInput name text c = G.inputMaybe getInput inputField text name
  where
    inputField i a = button_ [type_ "submit", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece a)] $ toHtml c

-- | create a  @\<button type=\"reset\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
buttonReset
  :: (Monad m, FormError error, Monad f)
  => String
  -> HtmlT f ()
  -> Form m input error (HtmlT f ()) () ()
buttonReset name c = G.inputNoData inputField Nothing name
  where
    inputField i a = button_ [type_ "reset", id_ (toPathPiece i), name_ (toPathPiece i)] c

-- | create a  @\<button type=\"button\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
button
  :: (Monad m, FormError error, Monad f)
  => String
  -> HtmlT f ()
  -> Form m input error (HtmlT f ()) () ()
button name c = G.inputNoData inputField Nothing name
  where
    inputField i a = button_ [type_ "button", id_ (toPathPiece i), name_ (toPathPiece i)] c

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
label
  :: (Monad m, Monad f)
  => HtmlT f ()
  -> Form m input error (HtmlT f ()) () ()
label c = G.label mkLabel
  where
    mkLabel i = label_ [for_ (toPathPiece i)] c

arbitraryHtml :: Monad m => view -> Form m input error view () ()
arbitraryHtml wrap =
  Form $ do
    id' <- getFormId
    pure
      ( View (const $ wrap)
      , pure
        ( Ok $ Proved
          { proofs = ()
          , pos = unitRange id'
          , unProved = ()
          }
        )
      )

inputInt
  :: (Monad m, FormError err, Applicative f)
  => (input -> Either err Int)
  -> String
  -> Int
  -> Form m input err (HtmlT f ()) () Int
inputInt getInput name initialValue = G.input getInput inputField initialValue name
  where
    min = toPathPiece (minBound :: Int)
    {-# INLINE min #-}
    max = toPathPiece (maxBound :: Int)
    {-# INLINE max #-}
    inputField i a =
      input_
        [ type_ "number"
        , id_ (toPathPiece i)
        , name_ (toPathPiece i)
        , value_ (toPathPiece a)
        , min_ min
        , max_ max
        ]

inputDouble
  :: (Monad m, FormError err, Applicative f)
  => (input -> Either err Double)
  -> String
  -> Double
  -> Form m input err (HtmlT f ()) () Double
inputDouble getInput name initialValue = G.input getInput inputField initialValue name
  where
    inputField i a = input_ [type_ "number", step_ "any", id_ (toPathPiece i), name_ (toPathPiece i), value_ (T.pack $ show a)]

-- | Create a single @\<input type=\"checkbox\"\>@ element
--
-- returns a 'Bool' indicating if it was checked or not.
--
-- see also 'inputCheckboxes'
-- FIXME: Should this built on something in Generalized?
inputCheckbox
  :: forall x error input m f. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, Applicative f)
  => Bool -- ^ initially checked
  -> Form m input error (HtmlT f ()) () Bool
inputCheckbox initiallyChecked =
  Form $ do
    i <- getFormId
    v <- getFormInput' i
    case v of
      Default -> mkCheckbox i initiallyChecked
      Missing -> mkCheckbox i False -- checkboxes only appear in the submitted data when checked
      (Found input) ->
        case getInputString input of
          (Right _) -> mkCheckbox i True
          (Left (e :: error)) -> mkCheckbox i False
  where
    mkCheckbox i checked =
      let checkbox =
            input_ $
              (if checked then (:) checked_ else id)
                [type_ "checkbox", id_ (toPathPiece i), name_ (toPathPiece i), value_ (toPathPiece i)]
       in pure
            ( View $ const $ checkbox
            , pure $
              Ok
                ( Proved
                  { proofs = ()
                  , pos = unitRange i
                  , unProved = if checked then True else False
                  }
                )
            )

-- | Create a group of @\<input type=\"checkbox\"\>@ elements
--
inputCheckboxes
  :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToHtml lbl, Monad f)
  => String
  -> [(a, lbl)] -- ^ value, label, initially checked
  -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
  -> Form m input error (HtmlT f ()) () [a]
inputCheckboxes name choices isChecked = G.inputMulti choices mkCheckboxes isChecked name
  where
    mkCheckboxes nm choices' = mconcat $ concatMap (mkCheckbox nm) choices'
    mkCheckbox nm (i, val, lbl, checked) =
      [ input_ $
          ( (if checked then (checked_ :) else id)
            [type_ "checkbox", id_ (toPathPiece i), name_ (toPathPiece nm), value_ (toPathPiece val)]
          )
      , label_ [for_ (toPathPiece i)] $ toHtml lbl
      ]

-- | Create a group of @\<input type=\"radio\"\>@ elements
inputRadio
  :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToHtml lbl, Monad f)
  => String
  -> [(a, lbl)] -- ^ value, label, initially checked
  -> (a -> Bool) -- ^ isDefault
  -> Form m input error (HtmlT f ()) () a
inputRadio name choices isDefault =
  G.inputChoice isDefault choices mkRadios name
  where
    mkRadios nm choices' = mconcat $ concatMap (mkRadio nm) choices'
    mkRadio nm (i, val, lbl, checked) =
      [ input_ $
          (if checked then (checked_ :) else id)
            [type_ "radio", id_ (toPathPiece i), name_ (toPathPiece nm), value_ (toPathPiece val)]
      , label_ [for_ (toPathPiece i)] $ toHtml lbl
      , br_ []
      ]

-- | create @\<select\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- see also: 'selectMultiple'
select
  :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToHtml lbl, Monad f)
  => String
  -> [(a, lbl)] -- ^ value, label
  -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
  -> Form m input error (HtmlT f ()) () a
select name choices isDefault = G.inputChoice isDefault choices mkSelect name
  where
    mkSelect :: (ToHtml lbl, Monad f) => FormId -> [(a, Int, lbl, Bool)] -> HtmlT f ()
    mkSelect nm choices' =
      select_ [name_ (toPathPiece nm)] $
        traverse_ mkOption choices'
    mkOption :: (ToHtml lbl, Monad f) => (a, Int, lbl, Bool) -> HtmlT f ()
    mkOption (_, val, lbl, selected) =
      option_
        ( (if selected then ((:) (selected_ "selected")) else id)
          [value_ (toPathPiece val)]
        )
        (toHtml lbl)

-- | create @\<select multiple=\"multiple\"\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- This creates a @\<select\>@ element which allows more than one item to be selected.
selectMultiple
  :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToHtml lbl, Monad f)
  => String
  -> [(a, lbl)] -- ^ value, label, initially checked
  -> (a -> Bool) -- ^ isSelected initially
  -> Form m input error (HtmlT f ()) () [a]
selectMultiple name choices isSelected = G.inputMulti choices mkSelect isSelected name
  where
    mkSelect :: (ToHtml lbl, Monad f) => FormId -> [(a, Int, lbl, Bool)] -> HtmlT f ()
    mkSelect nm choices' =
      select_ [name_ (toPathPiece nm), multiple_ "multiple"] $
        traverse_ mkOption choices'
    mkOption :: (ToHtml lbl, Monad f) => (a, Int, lbl, Bool) -> HtmlT f ()
    mkOption (_, val, lbl, selected) =
      option_
        ( (if selected then ((:) (selected_ "selected")) else id)
          [value_ (toPathPiece val)]
        )
        (toHtml lbl)

{-
inputMultiSelectOptGroup :: (Functor m, XMLGenerator x, EmbedAsChild x groupLbl, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m, Applicative f) =>
                   [(groupLbl, [(a, lbl, Bool)])]  -- ^ value, label, initially checked
                -> Form m input error (HtmlT f ()) () [a]
inputMultiSelectOptGroup choices =
    G.inputMulti choices mkSelect
    where
      mkSelect nm choices' =
          [<select name=nm multiple="multiple">
            <% mapM mkOptGroup choices' %>
           </select>
          ]
      mkOptGroup (grpLabel, options) =
          <optgroup label=grpLabel>
           <% mapM mkOption options %>
          </optgroup>
      mkOption (_, val, lbl, selected) =
          <option value=val (if selected then ["selected" := "selected"] else [])>
           <% lbl %>
          </option>
-}

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- The @<\ul\>@ will have the attribute @class=\"ditto-error-list\"@.
errorList
  :: (Monad m, ToHtml error, Monad f)
  => Form m input error (HtmlT f ()) () ()
errorList = G.errors mkErrors
  where
    mkErrors :: Monad f => ToHtml a => [a] -> HtmlT f ()
    mkErrors [] = mempty
    mkErrors errs = ul_ [class_ "ditto-error-list"] $ traverse_ mkError errs
    mkError :: Monad f => ToHtml a => a -> HtmlT f ()
    mkError e = li_ [] $ toHtml e

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- Includes errors from child forms.
--
-- The @<\ul\>@ will have the attribute @class=\"ditto-error-list\"@.
childErrorList
  :: (Monad m, ToHtml error, Monad f)
  => Form m input error (HtmlT f ()) () ()
childErrorList = G.childErrors mkErrors
  where
    mkErrors :: Monad f => ToHtml a => [a] -> HtmlT f ()
    mkErrors [] = mempty
    mkErrors errs = ul_ [class_ "ditto-error-list"] $ traverse_ mkError errs
    mkError :: Monad f => ToHtml a => a -> HtmlT f ()
    mkError e = li_ [] $ toHtml e

-- | create a @\<br\>@ tag.
br :: (Monad m, Applicative f) => Form m input error (HtmlT f ()) () ()
br = view (br_ [])

-- | wrap a @\<fieldset class=\"ditto\"\>@ around a 'Form'
--
fieldset
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) proof a
  -> Form m input error (HtmlT f ()) proof a
fieldset frm = mapView (fieldset_ [class_ "ditto"]) frm

-- | wrap an @\<ol class=\"ditto\"\>@ around a 'Form'
ol
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) proof a
  -> Form m input error (HtmlT f ()) proof a
ol frm = mapView (ol_ [class_ "ditto"]) frm

-- | wrap a @\<ul class=\"ditto\"\>@ around a 'Form'
ul
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) proof a
  -> Form m input error (HtmlT f ()) proof a
ul frm = mapView (ul_ [class_ "ditto"]) frm

-- | wrap a @\<li class=\"ditto\"\>@ around a 'Form'
li
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) proof a
  -> Form m input error (HtmlT f ()) proof a
li frm = mapView (li_ [class_ "ditto"]) frm

-- | create @\<form action=action method=\"GET\" enctype=\"multipart/form-data\"\>@
formGenGET
  :: (Applicative m)
  => Text -- ^ action url
  -> [(Text, Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
formGenGET action hidden children = do
  form_ [action_ action, method_ "GET", enctype_ "multipart/form-data"] $
    traverse_ mkHidden hidden *>
    children
  where
    mkHidden (name, value) = input_ [type_ "hidden", name_ name, value_ value]

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
formGenPOST
  :: (Applicative m)
  => Text -- ^ action url
  -> [(Text, Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
formGenPOST action hidden children = do
  form_ [action_ action, method_ "POST", enctype_ "multipart/form-data"] $
    traverse_ mkHidden hidden *>
    children
  where
    mkHidden (name, value) = input_ [type_ "hidden", name_ name, value_ value]

-- | add an attribute to the 'Html' for a form element.
setAttr
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) proof a
  -> [Attribute]
  -> Form m input error (HtmlT f ()) proof a
setAttr form attr = mapView (\x -> x `with` attr) form
