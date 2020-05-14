module Main
  ( main
  ) where

import Prelude

import Control.Coroutine as Coroutine

import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)

import FFI.HTTPS as HTTPS

import Control.Tier2 as Tier2

{-- todo: read port, key, and cert from process args --}
main :: Effect Unit
main = void $ launchAff $ do
  server <- liftEffect $ HTTPS.createServer defaultOptions 
  _      <- liftEffect $ HTTPS.listen port $ server 
  _      <- Coroutine.runProcess $ Tier2.process server
  pure unit
  where port = 3000

defaultOptions :: HTTPS.Options
defaultOptions = HTTPS.Options { key : defaultKey, cert : defaultCert }
  where
    defaultCert = 
      """-----BEGIN CERTIFICATE-----
MIIDETCCAfkCFB41LbS4IhdudbCffrK7/m0vSf0HMA0GCSqGSIb3DQEBCwUAMEUx
CzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl
cm5ldCBXaWRnaXRzIFB0eSBMdGQwHhcNMjAwNTE0MjAxMTA3WhcNMjEwNTE0MjAx
MTA3WjBFMQswCQYDVQQGEwJBVTETMBEGA1UECAwKU29tZS1TdGF0ZTEhMB8GA1UE
CgwYSW50ZXJuZXQgV2lkZ2l0cyBQdHkgTHRkMIIBIjANBgkqhkiG9w0BAQEFAAOC
AQ8AMIIBCgKCAQEAq2tg7SYebJ2pGvWrvZgAl5041FvMjnMGPrwdo1MKxtFsTeWj
8D6nQHNYhVLQIHl6h+NCG5ecwZIIeWFYa8aznoa2P/DKvGuKdaQyhjHp6XsSMREA
EQsOBROM3d+PRpTlL2ZDbcVcOV2NG3AqqMF06JyCBAhCY9sW12Y5xDbsJI7bjCxa
y+daFi5kT5UTSW/qoYkNFokdOblXP6+Eoi3e9Y0RZCfqgW8+qcuYuLpHYft82viH
FAANGDw3JSSXDzUFHhvgr5b2IwPXXJYx9k/pzDby5aGFPBWNkAKZF2zJNzWX2rAq
R9G9WttWem/l5w9Un44NTq+xcWEGbA943+5dZwIDAQABMA0GCSqGSIb3DQEBCwUA
A4IBAQCJo9sZBzOPKOaVCTt9Ys4TFKGOKxrmLSMn5PNE+BwziHcJezFMJXAcRcUs
XiTkyCTM78TfE4SLCd3oOhwenWAUduOCN4e/lYNZGucMuwk335jkzvZQ36ugTfp6
/r+loUxqq2skWZsNZleR70uI1guOW60jY4IvnhiqNiNjNNSw+OAjUBbRz3Lmp2LG
ffFqCP+BvyGuiWj8AvCN4rgkAK4GwQFsL+AL0aeBGJRPY/IKuNpDR7NwtaQwdnzQ
5HH+GTqHrSve9moNDZ6HVBCQxS9pq16u0bUtDbX+0nwBkGyjWh7o08jPbbUNcul9
PwPyyQNakceCKT6WsFEH47udb/qB
-----END CERTIFICATE-----"""
    defaultKey =
      """-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCra2DtJh5snaka
9au9mACXnTjUW8yOcwY+vB2jUwrG0WxN5aPwPqdAc1iFUtAgeXqH40Ibl5zBkgh5
YVhrxrOehrY/8Mq8a4p1pDKGMenpexIxEQARCw4FE4zd349GlOUvZkNtxVw5XY0b
cCqowXTonIIECEJj2xbXZjnENuwkjtuMLFrL51oWLmRPlRNJb+qhiQ0WiR05uVc/
r4SiLd71jRFkJ+qBbz6py5i4ukdh+3za+IcUAA0YPDclJJcPNQUeG+CvlvYjA9dc
ljH2T+nMNvLloYU8FY2QApkXbMk3NZfasCpH0b1a21Z6b+XnD1Sfjg1Or7FxYQZs
D3jf7l1nAgMBAAECggEAWRUid9uZcAS2/uWw7IRzulbgU6upMFYiZrPICzp6nTv0
Nr8iZRWKQXowBIk208mNndTCZCh0NDw53ypxkwDeE4IacDwof6a7e8tq3kZOMyHc
IFefWzwNeyHf0OX/RUTXnbxdSkyIUt+FFz9BTBXNNZ0wC/xm1BHlWucrGgcYzR3+
vWddXnW/Z84PMB7dDbMz9/i9KXRbOTDCDH3tuK3ZgTaf80zGiv4Taxh7+PkVPDcX
ILF655dP1nf7IKtQKxetom877juGdED12qj/ljJPKEEKSxW46+y9F4Zx2ihR+FYC
i/7MlvKdDK/sR+M4RcsdguhRxc4rvUDIyrBR9eMGUQKBgQDWOzlCVE/1LNQwfA1E
+ORQd7eZ120uWILvwPflxFB4J1BohbYOPnEN/d7ru9R9B+xWQ1vafLFKFlgjuAWA
68G1XP9B7/Xfn3sCJD8l4wuVRDyoPtc6e4hTHQvDDgO5/F6ASJvKFyg4Nq0fVjQS
vegCXWaT02VMTlYHuSFuLHiKawKBgQDM11C7P2tF12BvCma1hUt5IGrHs5MkIXMN
yFmWILh6jRy8Rv68zIq3mcAIwSXYYg3sR7ib3biwl+iLNKsk8W+rBRmvlvf1o03T
rKzmWRjIsDR7Hhfog1L3lFs7eeMoc1meV9DfU9xgmdaBKpTZoeeZ1MoCFlAr1bF8
rlJDXAjv9QKBgQCLQA20/qbGf/Ag39GkPn/xig+ukOfoEyJxAyZflF6V60AJ4Lf4
xD+qT5Vh90JkTw0g+UGwYBLBTIrYFNqG9TeBUsScsIRKahiDyNm/H7i8upEO72Hp
Irj3BU9K50Ym01L94bRaBPlgqLGZR+grMoOnXdy6Q9N5aQXkmUKQoZlspwKBgFCb
0rs9A7viO6K21yBLqh2qchrQQfEP8LTVyNtZWP9xFIPK9GfbniMOMBHB4GC8hXob
5zv/DrT/spVFTqwlDqqgQTFmGYHil7fa1dd37FbdcGmauBHZzAZ9rUFEf+3lJrk3
GY0o9SpWn6EI3raeDgPtRul3WmlFmKjV+K1lRGcZAoGBAJro3BDyAalZ/J4x7Fp4
PD7R3OhHBLzmwVZiqRoiAvPT8iZwKiNyV0qm5GU14F+n6XZGrZQo7bx/c+JQTHs/
ohRGZYbmFynOdBOLHms/PrUyvudNhnJq/FXQNXPsNoGGqUB6X03G/ru9Na4N6Ziy
hzC9D2VfRdQv9EnQrCovEg3F
-----END PRIVATE KEY-----"""
