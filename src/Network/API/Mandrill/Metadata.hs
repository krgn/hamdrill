{-# LANGUAGE OverloadedStrings #-}

module Network.API.Mandrill.Metadata where

import Network.API.Mandrill.Response
import Network.API.Mandrill.Types
import Network.API.Mandrill.Utils

-- | Get the list of custom metadata fields indexed for the account.
list :: (MonadIO m) => MandrillT m (Either ApiError [Metadata])
list  = performRequest "/metadata/list.json" []


-- | Add a new custom metadata field to be indexed for the account.
delete :: (MonadIO m) => Name -> MandrillT m (Either ApiError Metadata)
delete n = performRequest "/metadata/delete.json" [ "name" .= n ]


-- | Update an existing custom metadata field.
add :: (MonadIO m) => Metadata ->  MandrillT m (Either ApiError Metadata)
add m  = performRequest "/metadata/add.json" $
             [ "name"          .= _metadata_name m
             , "view_template" .= _metadata_view_template m ]


-- | Delete an existing custom metadata field. Deletion isn't instataneous, 
-- and /metadata/list will continue to return the field until the  
-- asynchronous deletion process is complete.
update :: (MonadIO m) => Metadata ->  MandrillT m (Either ApiError Metadata)
update m  = performRequest "/metadata/update.json" $
                 [ "name"          .= _metadata_name m
                 , "view_template" .= _metadata_view_template m ]
