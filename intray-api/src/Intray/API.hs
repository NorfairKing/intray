{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API
  ( module Intray.API,
    module Intray.API.AccessKeySecret,
    module Intray.API.AccessKeyUUID,
    module Intray.API.AccountUUID,
    module Intray.API.HashedPassword,
    module Intray.API.ItemType,
    module Intray.API.ItemUUID,
    module Intray.API.Permission,
    module Intray.API.Username,
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Exception
import Control.Monad.Logger
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import Data.Int
import Data.Mergeless
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Intray.API.AccessKeySecret
import Intray.API.AccessKeyUUID
import Intray.API.AccountUUID
import Intray.API.HashedPassword
import Intray.API.ItemType
import Intray.API.ItemUUID
import Intray.API.Permission
import Intray.API.Username
import Network.Wai
import OptEnvConf
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import Servant.Auth.Server.Internal.Class

intrayAPI :: Proxy IntrayAPI
intrayAPI = Proxy

type IntrayAPI = ToServantApi IntraySite

data IntraySite route = IntraySite
  { openSite :: !(route :- ToServantApi IntrayOpenSite),
    adminSite :: !(route :- "admin" :> ToServantApi IntrayAdminSite)
  }
  deriving (Generic)

data IntrayOpenSite route = IntrayOpenSite
  { protectedSite :: !(route :- ToServantApi IntrayProtectedSite),
    publicSite :: !(route :- ToServantApi IntrayPublicSite)
  }
  deriving (Generic)

data IntrayPublicSite route = IntrayPublicSite
  { postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    getPricing :: !(route :- GetPricing),
    postStripeHook :: !(route :- PostStripeHook)
  }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type PostRegister =
  "register"
    :> ReqBody '[JSON] Registration
    :> Verb 'POST 204 '[JSON] NoContent

data Registration = Registration
  { registrationUsername :: Username,
    registrationPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Registration)

instance Validity Registration

instance HasCodec Registration where
  codec =
    object "Registration" $
      Registration
        <$> requiredField "name" "Username"
          .= registrationUsername
        <*> requiredField "password" "Password"
          .= registrationPassword

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "Username"
          .= loginFormUsername
        <*> requiredField "password" "Password"
          .= loginFormPassword

type GetPricing =
  "pricing"
    :> Get '[JSON] (Maybe Pricing)

data Pricing = Pricing
  { pricingPlan :: !Text,
    pricingPrice :: !Text,
    pricingStripePublishableKey :: !Text,
    pricingMaxItemsFree :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Pricing)

instance Validity Pricing

instance HasCodec Pricing where
  codec =
    object "Pricing" $
      Pricing
        <$> requiredField "plan" "stripe plan"
          .= pricingPlan
        <*> requiredField "price" "price"
          .= pricingPrice
        <*> requiredField "publishable-key" "publishable key"
          .= pricingStripePublishableKey
        <*> requiredField "max-items-free" "how many items a free account can have"
          .= pricingMaxItemsFree

type PostStripeHook =
  "stripe"
    :> ReqBody '[JSON] JSON.Value
    :> Verb 'POST 204 '[JSON] NoContent

type ProtectAPI = Auth '[JWT, IntrayAccessKey] AuthCookie

data IntrayAccessKey

newtype AccessKeyEnv = AccessKeyEnv
  { accessKeyEnvAuthenticate :: Username -> Text -> IO (AuthResult AuthCookie)
  }

instance IsAuth IntrayAccessKey AuthCookie where
  type AuthArgs IntrayAccessKey = '[AccessKeyEnv]
  runAuth _ _ AccessKeyEnv {..} = AuthCheck $ \req ->
    case (,) <$> lookup "Username" (requestHeaders req) <*> lookup "Access-Key" (requestHeaders req) of
      Nothing -> pure Indefinite
      Just (unbs, akbs) ->
        case (,)
          <$> (left displayException (TE.decodeUtf8' unbs) >>= parseUsernameWithError)
          <*> left displayException (TE.decodeUtf8' akbs) of
          Left _ -> pure Indefinite
          Right (un, ak) -> accessKeyEnvAuthenticate un ak

data AuthCookie = AuthCookie
  { authCookieUserUUID :: AccountUUID,
    authCookiePermissions :: Set Permission,
    authCookieAccessKeyName :: Maybe Text
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AuthCookie)

instance HasCodec AuthCookie where
  codec =
    object "AuthCookie" $
      AuthCookie
        <$> requiredField "uuid" "user uuid"
          .= authCookieUserUUID
        <*> optionalFieldWithOmittedDefault "permissions" S.empty "permissions"
          .= authCookiePermissions
        <*> optionalField "access-key" "access key name" .= authCookieAccessKeyName

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data IntrayProtectedSite route = IntrayProtectedSite
  { protectedItemSite :: !(route :- "intray" :> ToServantApi IntrayProtectedItemSite),
    protectedAccountSite :: !(route :- "account" :> ToServantApi IntrayProtectedAccountSite),
    protectedAccessKeySite :: !(route :- "access-key" :> ToServantApi IntrayProtectedAccessKeySite),
    getPermissions :: !(route :- GetPermissions)
  }
  deriving (Generic)

data IntrayProtectedItemSite route = IntrayProtectedItemSite
  { getShowItem :: !(route :- GetShowItem),
    getIntraySize :: !(route :- GetIntraySize),
    getItemUUIDs :: !(route :- GetItemUUIDs),
    getItems :: !(route :- GetItems),
    postAddItem :: !(route :- PostAddItem),
    getItem :: !(route :- GetItem),
    deleteItem :: !(route :- DeleteItem),
    postSync :: !(route :- PostSync)
  }
  deriving (Generic)

-- | The item is not guaranteed to be the same one for every call if there are multiple items available.
type GetShowItem =
  ProtectAPI
    :> "show-item"
    :> Get '[JSON] (Maybe (ItemInfo TypedItem))

-- | Show the number of items in the intray
type GetIntraySize =
  ProtectAPI
    :> "size"
    :> Get '[JSON] Int

-- | The order of the items is not guaranteed to be the same for every call.
type GetItemUUIDs =
  ProtectAPI
    :> "uuids"
    :> Get '[JSON] [ItemUUID]

-- | The order of the items is not guaranteed to be the same for every call.
type GetItems =
  ProtectAPI
    :> "items"
    :> Get '[JSON] [ItemInfo TypedItem]

type PostAddItem =
  ProtectAPI
    :> "item"
    :> ReqBody '[JSON] TypedItem
    :> Post '[JSON] ItemUUID

type GetItem =
  ProtectAPI
    :> "item"
    :> Capture "uuid" ItemUUID
    :> Get '[JSON] (ItemInfo TypedItem)

type DeleteItem =
  ProtectAPI
    :> "item"
    :> Capture "uuid" ItemUUID
    :> Delete '[JSON] NoContent

type PostSync =
  ProtectAPI
    :> "sync"
    :> ReqBody '[JSON] (SyncRequest Int64 ItemUUID (AddedItem TypedItem))
    :> Post '[JSON] (SyncResponse Int64 ItemUUID (AddedItem TypedItem))

data IntrayProtectedAccountSite route = IntrayProtectedAccountSite
  { getAccountInfo :: !(route :- GetAccountInfo),
    postChangePassphrase :: !(route :- PostChangePassphrase),
    deleteAccount :: !(route :- DeleteAccount),
    postInitiateStripeCheckoutSession :: !(route :- PostInitiateStripeCheckoutSession)
  }
  deriving (Generic)

type GetAccountInfo =
  ProtectAPI
    :> Get '[JSON] AccountInfo

type PostChangePassphrase =
  ProtectAPI
    :> "change-passphrase"
    :> ReqBody '[JSON] ChangePassphrase
    :> Verb 'POST 204 '[JSON] NoContent

type DeleteAccount =
  ProtectAPI
    :> Verb 'DELETE 204 '[JSON] NoContent

type PostInitiateStripeCheckoutSession =
  ProtectAPI
    :> "checkout"
    :> "stripe"
    :> "session"
    :> ReqBody '[JSON] InitiateStripeCheckoutSession
    :> Post '[JSON] InitiatedCheckoutSession

data AccountInfo = AccountInfo
  { accountInfoUUID :: AccountUUID,
    accountInfoUsername :: Username,
    accountInfoCreatedTimestamp :: UTCTime,
    accountInfoLastLogin :: Maybe UTCTime,
    accountInfoAdmin :: Bool,
    accountInfoCount :: Int,
    accountInfoStatus :: PaidStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AccountInfo)

instance Validity AccountInfo

instance HasCodec AccountInfo where
  codec =
    object "AccountInfo" $
      AccountInfo
        <$> requiredField "uuid" "account uuid"
          .= accountInfoUUID
        <*> requiredField "username" "account username"
          .= accountInfoUsername
        <*> requiredField "created" "creation time"
          .= accountInfoCreatedTimestamp
        <*> requiredField "last-login" "last login time"
          .= accountInfoLastLogin
        <*> requiredField "admin" "whether the user is an admin"
          .= accountInfoAdmin
        <*> requiredField "count" "how many items the user has in their intray"
          .= accountInfoCount
        <*> requiredField "status" "paid status of the account"
          .= accountInfoStatus

data PaidStatus
  = HasNotPaid Int -- Number of extra items that they're still allowed
  | HasPaid UTCTime
  | NoPaymentNecessary
  deriving (Show, Eq, Generic)

instance Validity PaidStatus

instance HasCodec PaidStatus where
  codec =
    object "PaidStatus" $
      dimapCodec f g $
        eitherCodec (requiredField "items-left" "how many free items the user has left") $
          eitherCodec
            (requiredField "until" "when the subscription expires")
            (pure NoPaymentNecessary)
    where
      f = \case
        Left i -> HasNotPaid i
        Right (Left t) -> HasPaid t
        Right (Right ps) -> ps
      g = \case
        HasNotPaid i -> Left i
        HasPaid t -> Right (Left t)
        NoPaymentNecessary -> Right (Right NoPaymentNecessary)

data ChangePassphrase = ChangePassphrase
  { changePassphraseOld :: Text,
    changePassphraseNew :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ChangePassphrase)

instance Validity ChangePassphrase

instance HasCodec ChangePassphrase where
  codec =
    object "ChangePassphrase" $
      ChangePassphrase
        <$> requiredField "old-passphrase" "old passphrase"
          .= changePassphraseOld
        <*> requiredField "new-passphrase" "new passphrase"
          .= changePassphraseNew

data InitiateStripeCheckoutSession = InitiateStripeCheckoutSession
  { initiateStripeCheckoutSessionSuccessUrl :: Text,
    initiateStripeCheckoutSessionCanceledUrl :: Text
  }
  deriving (FromJSON, ToJSON) via (Autodocodec InitiateStripeCheckoutSession)

instance HasCodec InitiateStripeCheckoutSession where
  codec =
    object "InitiateStripeCheckoutSession" $
      InitiateStripeCheckoutSession
        <$> requiredField "success" "success url"
          .= initiateStripeCheckoutSessionSuccessUrl
        <*> requiredField "canceled" "canceled url"
          .= initiateStripeCheckoutSessionCanceledUrl

data InitiatedCheckoutSession = InitiatedCheckoutSession
  { initiatedCheckoutSessionId :: Text,
    initiatedCheckoutSessionCustomerId :: Text
  }
  deriving (FromJSON, ToJSON) via (Autodocodec InitiatedCheckoutSession)

instance HasCodec InitiatedCheckoutSession where
  codec =
    object "InitiatedCheckoutSession" $
      InitiatedCheckoutSession
        <$> requiredField "session" "session identifier"
          .= initiatedCheckoutSessionId
        <*> requiredField "customer" "customer identifier"
          .= initiatedCheckoutSessionCustomerId

data IntrayProtectedAccessKeySite route = IntrayProtectedAccessKeySite
  { postAddAccessKey :: !(route :- PostAddAccessKey),
    getAccessKey :: !(route :- GetAccessKey),
    getAccessKeys :: !(route :- GetAccessKeys),
    deleteAccessKey :: !(route :- DeleteAccessKey)
  }
  deriving (Generic)

type PostAddAccessKey =
  ProtectAPI :> ReqBody '[JSON] AddAccessKey :> Post '[JSON] AccessKeyCreated

type GetAccessKey = ProtectAPI :> Capture "uuid" AccessKeyUUID :> Get '[JSON] AccessKeyInfo

type GetAccessKeys = ProtectAPI :> Get '[JSON] [AccessKeyInfo]

type DeleteAccessKey = ProtectAPI :> Capture "uuid" AccessKeyUUID :> Delete '[JSON] NoContent

data AccessKeyInfo = AccessKeyInfo
  { accessKeyInfoUUID :: AccessKeyUUID,
    accessKeyInfoName :: Text,
    accessKeyInfoCreatedTimestamp :: UTCTime,
    accessKeyInfoPermissions :: Set Permission
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AccessKeyInfo)

instance HasCodec AccessKeyInfo where
  codec =
    object "AccessKeyInfo" $
      AccessKeyInfo
        <$> requiredField "uuid" "access key uuid"
          .= accessKeyInfoUUID
        <*> requiredField "name" "access key name"
          .= accessKeyInfoName
        <*> requiredField "created" "creation time"
          .= accessKeyInfoCreatedTimestamp
        <*> requiredField "permissions" "permissions"
          .= accessKeyInfoPermissions

data AddAccessKey = AddAccessKey
  { addAccessKeyName :: Text,
    addAccessKeyPermissions :: Set Permission
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AddAccessKey)

instance HasCodec AddAccessKey where
  codec =
    object "AddAccessKey" $
      AddAccessKey
        <$> requiredField "name" "access key name"
          .= addAccessKeyName
        <*> requiredField "permissions" "access key permissions"
          .= addAccessKeyPermissions

data AccessKeyCreated = AccessKeyCreated
  { accessKeyCreatedCreatedTimestamp :: UTCTime,
    accessKeyCreatedKey :: AccessKeySecret,
    accessKeyCreatedUUID :: AccessKeyUUID
  }
  deriving (FromJSON, ToJSON) via (Autodocodec AccessKeyCreated)

instance HasCodec AccessKeyCreated where
  codec =
    object "AccessKeyCreated" $
      AccessKeyCreated
        <$> requiredField "created" "created timestamp"
          .= accessKeyCreatedCreatedTimestamp
        <*> requiredField "secret" "access key secret"
          .= accessKeyCreatedKey
        <*> requiredField "uuid" "access key uuid"
          .= accessKeyCreatedUUID

type GetPermissions = ProtectAPI :> "permissions" :> Get '[JSON] (Set Permission)

data TypedItem = TypedItem
  { itemType :: ItemType,
    itemData :: ByteString
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TypedItem)

instance Validity TypedItem

instance HasCodec TypedItem where
  codec =
    object "TypedItem" $
      TypedItem
        <$> requiredField "type" "type of the item data"
          .= itemType
        <*> requiredFieldWith
          "data"
          ( bimapCodec
              (Base64.decode . SB8.pack)
              (SB8.unpack . Base64.encode)
              codec
          )
          "base64-encoded data"
          .= itemData

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
  case itemType of
    TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData
    ImageItem it -> pure $ CaseImageItem it itemData

data TypedItemCase
  = CaseTextItem Text
  | CaseImageItem ImageType ByteString

data AddedItem a = AddedItem
  { addedItemContents :: a,
    addedItemCreated :: UTCTime,
    addedItemAccessKeyName :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Validity a) => Validity (AddedItem a)

instance (HasCodec a) => HasCodec (AddedItem a) where
  codec =
    object "AddedItem" $
      AddedItem
        <$> requiredField "contents" "the item itself"
          .= addedItemContents
        <*> requiredField "created" "creation timestamp"
          .= addedItemCreated
        <*> optionalField "access-key" "access key used to create this item"
          .= addedItemAccessKeyName

data ItemInfo a = ItemInfo
  { itemInfoIdentifier :: ItemUUID,
    itemInfoContents :: a,
    itemInfoCreated :: UTCTime,
    itemInfoAccessKeyName :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemInfo a))

instance (Validity a) => Validity (ItemInfo a)

instance (HasCodec a) => HasCodec (ItemInfo a) where
  codec =
    object "ItemInfo" $
      ItemInfo
        <$> requiredField "id" "uuid"
          .= itemInfoIdentifier
        <*> requiredField "contents" "the item itself"
          .= itemInfoContents
        <*> requiredField "created" "creation timestamp"
          .= itemInfoCreated
        <*> optionalField "access-key" "access key used to create this item"
          .= itemInfoAccessKeyName

data IntrayAdminSite route = IntrayAdminSite
  { adminGetStats :: !(route :- AdminGetStats),
    adminDeleteAccount :: !(route :- AdminDeleteAccount),
    adminGetAccount :: !(route :- AdminGetAccount),
    adminGetAccounts :: !(route :- AdminGetAccounts),
    adminPutUserSubscription :: !(route :- PutUserSubscription)
  }
  deriving (Generic)

type AdminGetStats =
  ProtectAPI
    :> "stats"
    :> Get '[JSON] AdminStats

type AdminDeleteAccount =
  ProtectAPI
    :> "account"
    :> Capture "username" Username
    :> Verb 'DELETE 204 '[JSON] NoContent

type AdminGetAccount =
  ProtectAPI
    :> "account"
    :> Capture "username" Username
    :> Get '[JSON] AccountInfo

type AdminGetAccounts =
  ProtectAPI
    :> "accounts"
    :> Get '[JSON] [AccountInfo]

type PutUserSubscription =
  ProtectAPI
    :> "accounts"
    :> Capture "username" Username
    :> ReqBody '[JSON] UTCTime
    :> Verb 'PUT 204 '[JSON] NoContent

data AdminStats = AdminStats
  { adminStatsNbAccounts :: !Word,
    adminStatsSubscribedUsers :: !Word,
    adminStatsNbItems :: !Word,
    adminStatsActiveUsers :: !ActiveUsers
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AdminStats)

instance Validity AdminStats

instance HasCodec AdminStats where
  codec =
    object "AdminStats" $
      AdminStats
        <$> requiredField "accounts" "how many accounts there are"
          .= adminStatsNbAccounts
        <*> requiredField "subscribed-user" "how many of those are subscribed"
          .= adminStatsSubscribedUsers
        <*> requiredField "items" "total number of items that users have in their intrays"
          .= adminStatsNbItems
        <*> requiredField "active-users" "X-active users stats"
          .= adminStatsActiveUsers

data ActiveUsers = ActiveUsers
  { activeUsersDaily :: !Word,
    activeUsersWeekly :: !Word,
    activeUsersMonthly :: !Word,
    activeUsersYearly :: !Word
  }
  deriving stock (Show, Eq, Generic)

instance Validity ActiveUsers

instance HasCodec ActiveUsers where
  codec =
    object "ActiveUsers" $
      ActiveUsers
        <$> requiredField "daily" "daily active users"
          .= activeUsersDaily
        <*> requiredField "weekly" "weekly active users"
          .= activeUsersWeekly
        <*> requiredField "monthly" "monthly active users"
          .= activeUsersMonthly
        <*> requiredField "yearly" "yearly active users"
          .= activeUsersYearly

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

instance HasParser LogLevel where
  settingsParser =
    setting
      [ help "minimal severity for log message",
        reader $
          maybeReader $
            \case
              "Debug" -> Just LevelDebug
              "Info" -> Just LevelInfo
              "Warn" -> Just LevelWarn
              "Error" -> Just LevelError
              _ -> Nothing,
        metavar "LOG_LEVEL",
        name "log-level",
        value LevelInfo
      ]
