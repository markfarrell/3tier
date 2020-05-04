module Main
  ( main
  ) where

import Prelude

import Control.Coroutine as Coroutine

import Effect (Effect)
import Effect.Aff (forkAff, supervise, launchAff)
import Effect.Class (liftEffect)

import FFI.HTTPS as HTTPS

import Control.Tier2 as Tier2

main :: Effect Unit
main = void $ launchAff $ supervise $ do
  server <- liftEffect $ HTTPS.createServer defaultOptions 
  _      <- forkAff (Coroutine.runProcess $ Tier2.process server)
  _      <- liftEffect $ HTTPS.listen port $ server 
  pure unit
  where port = 3000

defaultOptions :: HTTPS.Options
defaultOptions = HTTPS.Options { key : defaultKey, cert : defaultCert }
  where
    defaultCert = 
      """-----BEGIN CERTIFICATE-----
      MIIDETCCAfkCFEKGMHoJR2bS0DelRo3xde1vtu2HMA0GCSqGSIb3DQEBCwUAMEUx
      CzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl
      cm5ldCBXaWRnaXRzIFB0eSBMdGQwHhcNMjAwNTA0MTc1NDI0WhcNNDcwOTE5MTc1
      NDI0WjBFMQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UE
      CgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIIBIjANBgkqhkiG9w0BAQEFAAOC
      AQ8AMIIBCgKCAQEAvFh4t49yDbEvw198YnFrW/OnEbedekPPQj7fuMvWwoodECBL
      EhGPKphuh3yrHPHOKxm46PqHIfnLrUYklizQ7mFqJ2xJE9L6wEOzCtX9exj//JmP
      h2BhohpokbVVOuRlRulZ9DUrNd9sc9X038xV3scQ1Nd7mtjxYUHHlKQnPMyftu39
      uQQJIImt50XyAIXys/Cij0+dgqVU0RPJfBd7XJrizfxPITKmo3QALKiQayrq7gto
      xnWGIxND2X1JlTpwPIHr1tMD8sorvE857Urb2Pepbxbridlwf/VGfExOTB7gkAXX
      3g9y70guk1mj10fmymo/CfQIxkCzWkTQ5v7WpQIDAQABMA0GCSqGSIb3DQEBCwUA
      A4IBAQAJ+MLcDL7JvHUk4lDdBsshiKHsjQ8sVnCKW11YR/FT3Y+kvm8DUYnIn0Bd
      0StKgvJD8dot4sSnJR5yx3S6+7Z8WiEIOPIZmbX52yqQiXp0FGqi/7d9O4eg17uz
      1qRwaK5XbYf9tQ5sKGhRhLhKh3i9Jwayq7hVfD6xA2ZpyBFmI8f5SPOXjChBWX1k
      I5AM1/d78Kyy9+e8kYOT0WzT71+/nnGxeokJ3Mx6IfCE0rfRJCQqsfUnBoi34vp3
      w2K2w0hLvTMzkI0t9eJeaQKkJU3piJrL7NDXQ8S3bliR6DsIm9GKlnNyaVMewZ8r
      PYJFn2mIGgemTSlcVBs6Rnpmk+Ew
      -----END CERTIFICATE-----"""
    defaultKey =
      """-----BEGIN RSA PRIVATE KEY-----
      MIIEpQIBAAKCAQEAvFh4t49yDbEvw198YnFrW/OnEbedekPPQj7fuMvWwoodECBL
      EhGPKphuh3yrHPHOKxm46PqHIfnLrUYklizQ7mFqJ2xJE9L6wEOzCtX9exj//JmP
      h2BhohpokbVVOuRlRulZ9DUrNd9sc9X038xV3scQ1Nd7mtjxYUHHlKQnPMyftu39
      uQQJIImt50XyAIXys/Cij0+dgqVU0RPJfBd7XJrizfxPITKmo3QALKiQayrq7gto
      xnWGIxND2X1JlTpwPIHr1tMD8sorvE857Urb2Pepbxbridlwf/VGfExOTB7gkAXX
      3g9y70guk1mj10fmymo/CfQIxkCzWkTQ5v7WpQIDAQABAoIBAQCuY9h6VcZL6TiJ
      VLstx0iZnjOeEv4ru+eGtgKd75cND6azTMNVmo6N6Q/GQWeKaFX10F5G0dkXW1v8
      NwsfRaW6YtZQZFIRmfL03HpF2TdXqCE1uNWNim88EemqqFSfL+SaUOwqUqSMwC+B
      3aRbJ66GlJkACfj3ulFdNPPfrFJsVoykyCxMBtvjKa7N8+IdrPbYQ+2930sRVKGO
      M+txXwGPv/Un+YPh+aRF6VHz5eZ/oCzepcN5gwwL7bcJCN6C2RNiSvfpBLLan5Pw
      oFQCEtX9nmC7R7CEips9rBMJKwntcq0D4ynVlFePelwzc0vGNs6pTA1+7gKV+597
      oOcV9OONAoGBAPoSL51Uir1Fg9U9Xk6hqpy8M2M3thHaoosSVyrHWch48GFDsAtZ
      /M4U4Dnecvo6Bvsm3r5Uq6Q60trLrY5a1/0+6QpvajzO9tHMc5poeFcBWqNIzGCF
      Vw8EFphHGqaQpQU6ZJKwzZLIwK/sDVRtCaJTDI8zUh0Mgi/CdXVckZ+nAoGBAMDP
      pBoDjhdPfhtsWqZ9daGjmuGVtFBgIwHkwBBgLYWrUVnSFyAk7auAkYFoWo71cAH1
      H29n2eYYufdhQD53mL1e1ImV9xhJcVI/3J23OYy9+n5w8et++dDC9HwB6P2PuYQp
      4nfuCcG5s63uFChCIf92ohMAQL0r23C1a4VyTcDTAoGBAJHpyp6ildGanjHufPik
      nEiYB+P8vd/tEqrTKMltSJYQ+zHFrRLyjvAwfiVGBxC6ixhAtfeYsA5DxISmE77c
      gurhqljMadDRcnM4OLv/+TfLMnfUImyvYP0CBa0U0BggjileyRBO2aSsPsJnmd+J
      P/XXP5zO9Tq98bisXpz8RnxBAoGBAJWcSSSaX5Y864iR8mE/9pffIF4s0MaRYnik
      rFfImWogPjapK6KYpQcv8zfr5mEJl8kdEDBc8Bp4U0zeXIBcQtumitrbz+k7i8Ab
      p6+FrhVEkOut7joiw+u2awkSXlzqOjSQX5dZ0J5O31p145Q9hK36utZtms5vCo6R
      Z4Yb5LNbAoGAb1sHKnOWEUy5tK39iAH8xp3+HuBcwJ4mv1KDH3AsUING3xzCHUIw
      sE9Li+H5WwFSVtMUUVPvsGxpa2mDLv5qoPhxmuEEOU93fje+sCVJrWC42ZcVizaz
      pRp11Eig4eBtPq14juhbGTRkQM6Q6qKxYvz38PFbMk5KVHwULwPSdUY=
      -----END RSA PRIVATE KEY-----"""
