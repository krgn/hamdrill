{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Templates where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Add a new template
add :: (MonadIO m) => 
      Template -> 
      Publish -> 
      MandrillT m (Either ApiError Template)
add t p =
  performRequest "/templates/add.json" $ 
           [ "publish"    .= p
           , "name"       .= _tmpl_name       t 
           , "from_email" .= _tmpl_from_email t 
           , "from_name"  .= _tmpl_from_name  t 
           , "subject"    .= _tmpl_subject    t 
           , "code"       .= _tmpl_code       t
           , "text"       .= _tmpl_text       t
           , "labels"     .= _tmpl_labels     t ]


-- | Get the information for an existing template
info :: (MonadIO m) => 
       Name -> 
       MandrillT m (Either ApiError Template)
info n = performRequest "/templates/info.json" [ "name" .= n ]


-- | Update the code for an existing template. If null is provided for any 
-- fields, the values will remain unchanged.
update :: (MonadIO m) =>
         Template -> 
         Publish -> 
         MandrillT m (Either ApiError Template)
update t p =
  performRequest "/templates/update.json" $ 
           [ "publish"    .= p
           , "name"       .= _tmpl_name       t 
           , "from_email" .= _tmpl_from_email t 
           , "from_name"  .= _tmpl_from_name  t 
           , "subject"    .= _tmpl_subject    t 
           , "code"       .= _tmpl_code       t
           , "text"       .= _tmpl_text       t
           , "labels"     .= _tmpl_labels     t ]


-- | Publish the content for the template. Any new messages sent using this  
-- template will start using the content that was previously in draft.
publish :: (MonadIO m) =>
          Name -> 
          MandrillT m (Either ApiError Template)
publish n = 
  performRequest "/templates/publish.json" [ "name" .= n ]


-- | Delete a template
delete :: (MonadIO m) =>
         Name -> 
         MandrillT m (Either ApiError Template)
delete n = 
  performRequest "/templates/delete.json" [ "name" .= n ]


-- | Return a list of all the templates available to this user
list :: (MonadIO m) => 
       Maybe Label -> 
       MandrillT m (Either ApiError [Template])
list l = performRequest "/templates/list.json" [ "label" .= l ]


-- | Return the recent history (hourly stats for the last 30 days) for a template
timeSeries :: (MonadIO m) =>
             Name -> 
             MandrillT m (Either ApiError [Stat])
timeSeries n = 
  performRequest "/templates/time-series.json" [ "name" .= n ]


-- | Inject content and optionally merge fields into a template, returning 
-- the HTML that results
render :: (MonadIO m) =>
         Name -> 
         [Content] ->  
         [MergeVar] -> 
         MandrillT m (Either ApiError HtmlFragment)
render n cs ms = 
  performRequest "/templates/render.json" $
          [ "template_name"    .= n
          , "template_content" .= cs
          , "merge_vars"       .= ms ]
