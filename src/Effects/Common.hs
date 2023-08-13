module Effects.Common where

import Type (HibetError(..))
import Utility ( showT )

import Effectful ( MonadIO(liftIO), type (:>), Eff, IOE )
import Effectful.Error.Static ( Error, catchError, throwError )

adapt ::
  ( IOE :> es
  , Error HibetError :> es
  )
  => IO a -> Eff es a
adapt m = catchError (liftIO m) $
  \stack err -> throwError $ EffectError (showT err) stack
