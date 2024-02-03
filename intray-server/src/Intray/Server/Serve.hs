module Intray.Server.Serve
  ( intrayServer,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import Intray.API
import Intray.Server.Handler
import Intray.Server.Types
import Servant.Auth.Server
import Servant.Server
import Servant.Server.Generic

intrayServer :: IntraySite (AsServerT IntrayHandler)
intrayServer =
  IntraySite
    { openSite = genericServerT intrayOpenServer,
      adminSite = genericServerT intrayAdminServer
    }

intrayOpenServer :: IntrayOpenSite (AsServerT IntrayHandler)
intrayOpenServer =
  IntrayOpenSite
    { protectedSite = genericServerT intrayProtectedServer,
      publicSite = genericServerT intrayPublicServer
    }

intrayProtectedServer :: IntrayProtectedSite (AsServerT IntrayHandler)
intrayProtectedServer =
  IntrayProtectedSite
    { protectedItemSite = genericServerT intrayProtectedItemServer,
      protectedAccountSite = genericServerT intrayProtectedAccountServer,
      protectedAccessKeySite = genericServerT intrayProtectedAccessKeyServer,
      getPermissions = withAuthResultAndPermission PermitGetPermissions serveGetPermissions
    }

intrayProtectedItemServer :: IntrayProtectedItemSite (AsServerT IntrayHandler)
intrayProtectedItemServer =
  IntrayProtectedItemSite
    { getShowItem = withAuthResultAndPermission PermitShow serveGetShowItem,
      getIntraySize = withAuthResultAndPermission PermitSize serveGetIntraySize,
      getItemUUIDs = withAuthResultAndPermission PermitGetItemUUIDs serveGetItemUUIDs,
      getItems = withAuthResultAndPermission PermitGetItems serveGetItems,
      postAddItem = withAuthResultAndPermission PermitAdd servePostAddItem,
      getItem = withAuthResultAndPermission PermitGetItem serveGetItem,
      deleteItem = withAuthResultAndPermission PermitDelete serveDeleteItem,
      postSync = withAuthResultAndPermission PermitSync servePostSync
    }

intrayProtectedAccountServer :: IntrayProtectedAccountSite (AsServerT IntrayHandler)
intrayProtectedAccountServer =
  IntrayProtectedAccountSite
    { getAccountInfo = withAuthResultAndPermission PermitGetAccountInfo serveGetAccountInfo,
      postChangePassphrase =
        withAuthResultAndPermission PermitPostChangePassphrase servePostChangePassphrase,
      deleteAccount = withAuthResultAndPermission PermitDeleteAccount serveDeleteAccount,
      postInitiateStripeCheckoutSession = withAuthResultAndPermission PermitInitiateCheckout servePostInitiateStripeCheckoutSession
    }

intrayProtectedAccessKeyServer :: IntrayProtectedAccessKeySite (AsServerT IntrayHandler)
intrayProtectedAccessKeyServer =
  IntrayProtectedAccessKeySite
    { postAddAccessKey = withAuthResultAndPermission PermitPostAddAccessKey servePostAddAccessKey,
      getAccessKey = withAuthResultAndPermission PermitGetAccessKey serveGetAccessKey,
      getAccessKeys = withAuthResultAndPermission PermitGetAccessKeys serveGetAccessKeys,
      deleteAccessKey = withAuthResultAndPermission PermitDeleteAccessKey serveDeleteAccessKey
    }

intrayAdminServer :: IntrayAdminSite (AsServerT IntrayHandler)
intrayAdminServer =
  IntrayAdminSite
    { adminGetStats = withAuthResultAndPermission PermitAdminGetStats serveAdminGetStats,
      adminDeleteAccount = withAuthResultAndPermission PermitAdminDeleteAccount serveAdminDeleteAccount,
      adminGetAccounts = withAuthResultAndPermission PermitAdminGetAccounts serveAdminGetAccounts,
      adminGetAccount = withAuthResultAndPermission PermitAdminGetAccounts serveAdminGetAccount,
      adminPutUserSubscription = withAuthResultAndPermission PermitAdminPutAccountSubscription serveAdminPutUserSubscription
    }

intrayPublicServer :: IntrayPublicSite (AsServerT IntrayHandler)
intrayPublicServer =
  IntrayPublicSite
    { postRegister = servePostRegister,
      postLogin = servePostLogin,
      getPricing = serveGetPricing,
      postStripeHook = servePostStripeHook
    }

withAuthResult :: (ThrowAll a) => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401

withAuthResultAndPermission ::
  (ThrowAll a) => Permission -> (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResultAndPermission p func =
  withAuthResult (\ac -> withPermission (authCookiePermissions ac) p (func ac))

withPermission :: (ThrowAll a) => Set Permission -> Permission -> a -> a
withPermission ps p func =
  if S.member p ps
    then func
    else throwAll err401
