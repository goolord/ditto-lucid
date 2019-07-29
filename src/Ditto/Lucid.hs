{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ditto.Lucid where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Lucid
import Ditto.Core
import Ditto.Generalized as G
import Ditto.Result (FormId)
import qualified Data.Text as T

encodeFormId :: FormId -> Text
encodeFormId = T.pack . show

-- | create @\<form action=action method=\"GET\" enctype=\"application/xxx-form-urlencoded\"\>@
formGenGET
  :: (Applicative m)
  => Text -- ^ action url
  -> [(Text, Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
formGenGET action hidden children = do
  form_ [action_ action, method_ "GET", enctype_ "application/xxx-form-urlencoded"] $
    traverse_ mkHidden hidden *>
    children
  where
  mkHidden (name, value) = input_ [type_ "hidden", name_ name, value_ value]

-- | create @\<form action=action method=\"POST\" enctype=\"application/xxx-form-urlencoded\"\>@
formGenPOST
  :: (Applicative m)
  => Text -- ^ action url
  -> [(Text, Text)] -- ^ hidden fields to add to form
  -> HtmlT m b
  -> HtmlT m b
formGenPOST action hidden children = do
  form_ [action_ action, method_ "POST", enctype_ "application/xxx-form-urlencoded"] $
    traverse_ mkHidden hidden *>
    children
  where
  mkHidden (name, value) = input_ [type_ "hidden", name_ name, value_ value]

-- | add an attribute to the 'Html' for a form element.
setAttr
  :: (Monad m, Functor m, Applicative f)
  => [Attribute]
  -> Form m input error (HtmlT f ()) a 
  -> Form m input error (HtmlT f ()) a
setAttr attr form = mapView (\x -> x `with` attr) form

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- The @<\ul\>@ will have the attribute @class=\"ditto-error-list\"@.
errorList
  :: (Monad m, ToHtml error, Monad f)
  => Form m input error (HtmlT f ()) ()
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
  => Form m input error (HtmlT f ()) ()
childErrorList = G.childErrors mkErrors
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
withErrors
  :: (Monad m, ToHtml error, Monad f)
  => [Attribute]
  -> ([error] -> HtmlT f ())
  -> Form m input error (HtmlT f ()) a
  -> Form m input error (HtmlT f ()) a
withErrors attrs renderError form = G.withErrors mkErrors form
  where
  mkErrors formlet errs = 
    formlet `with` attrs
    <* renderError errs

-- | create a @\<br\>@ tag.
br :: (Monad m, Applicative f) => Form m input error (HtmlT f ()) ()
br = view (br_ [])

-- | wrap a @\<fieldset class=\"ditto\"\>@ around a 'Form'
--
fieldset
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) a
  -> Form m input error (HtmlT f ()) a
fieldset frm = mapView (fieldset_ [class_ "ditto"]) frm

-- | wrap an @\<ol class=\"ditto\"\>@ around a 'Form'
ol
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) a
  -> Form m input error (HtmlT f ()) a
ol frm = mapView (ol_ [class_ "ditto"]) frm

-- | wrap a @\<ul class=\"ditto\"\>@ around a 'Form'
ul
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) a
  -> Form m input error (HtmlT f ()) a
ul frm = mapView (ul_ [class_ "ditto"]) frm

-- | wrap a @\<li class=\"ditto\"\>@ around a 'Form'
li
  :: (Monad m, Functor m, Applicative f)
  => Form m input error (HtmlT f ()) a
  -> Form m input error (HtmlT f ()) a
li frm = mapView (li_ [class_ "ditto"]) frm

