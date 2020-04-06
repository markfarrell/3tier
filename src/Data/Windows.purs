module Data.Windows
  ( EventCategory(..) 
  , EventID
  , EventType(..)
  , Event(..)
  , event
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Array as Array
import Data.Maybe(Maybe(..))
import Data.Either(Either(..))
import Data.Tuple(Tuple(..))

import Effect.Exception as Exception

import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.String (string)

import FFI.Date (Date)

import Parser (date, json, positiveInteger, positiveInteger)

data EventCategory = AccountLogon
  | AccountManagement
  | DirectoryService
  | LogonAndLogoff
  | ObjectAccess
  | NonAudit
  | PolicyChange
  | PrivilegeUse
  | ProcessTracking
  | System
  | Uncategorized

type EventID = Int

data EventType = Success | Failure

data Event = Event
  { eventCategory :: EventCategory
  , eventID       :: EventID
  , eventType     :: EventType
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  }

instance eqEventCategoryEvent :: Eq EventCategory where
  eq AccountLogon      AccountLogon      = true 
  eq AccountManagement AccountManagement = true
  eq DirectoryService  DirectoryService  = true 
  eq LogonAndLogoff    LogonAndLogoff    = true 
  eq ObjectAccess      ObjectAccess      = true 
  eq NonAudit          NonAudit          = true 
  eq PolicyChange      PolicyChange      = true 
  eq PrivilegeUse      PrivilegeUse      = true 
  eq ProcessTracking   ProcessTracking   = true 
  eq System            System            = true 
  eq Uncategorized     Uncategorized     = true 
  eq _                 _                 = false

event :: Parser String Event
event = do
  x <- json
  y <- pure $ read x
  case y of
    (Left _)  -> fail "Invalid input."
    (Right z) -> pure z

read :: Foreign -> Either Exception.Error Event
read = \x -> do
  eventCategory' <- read' "eventCategory" eventCategory $ x
  eventID'       <- read' "eventID" eventID $ x
  eventType'     <- read' "eventType" eventType $ x
  startTime'     <- read' "startTime" date $ x
  duration'      <- read' "duration"  positiveInteger $ x
  endTime'       <- read' "endTime" date $ x
  case eventID' of
    (Tuple eventCategory'' eventID'') -> do
      case eventCategory' == eventCategory'' of
        true  -> pure $ Event
          { eventCategory : eventCategory''
          , eventID       : eventID''
          , eventType     : eventType'
          , startTime     : startTime'
          , duration      : duration'
          , endTime       : endTime'
          }
        false -> Left (Exception.error "Invalid input.")

read' :: forall a. String -> Parser String a -> Foreign -> Either Exception.Error a
read' = \x y z -> do
  result  <- runExcept' (z ! x >>= Foreign.readString)
  result' <- run result y
  pure result'
  where 
    runExcept' = \x -> do
      result <- pure $ runExcept x
      case result of
        (Left _)    -> Left (Exception.error "Invalid raw input.")
        (Right raw) -> pure raw 
    run = \x y -> do
      result <- pure $ runParser x y
      case result of
        (Left _)    -> Left (Exception.error "Invalid input.")
        (Right val) -> pure val

eventID :: Parser String (Tuple EventCategory EventID)
eventID = choice (eventID' <$> eventCategories)
  where
    eventID' = \eventCategory' -> do
      result <- positiveInteger
      case Array.elemIndex result (eventIDs eventCategory') of
        (Just _)  -> pure (Tuple eventCategory' result)
        (Nothing) -> fail "Invalid input."

eventCategory :: Parser String EventCategory
eventCategory = do
  result <- choice (string <$> writeEventCategory <$> eventCategories)
  case readEventCategory result of
    (Left _)               -> fail "Invalid input."
    (Right eventCategory') -> pure eventCategory'

eventType :: Parser String EventType
eventType = choice (eventType' <$> [Success, Failure])
  where
    eventType' = \x -> do
       _ <- string (writeEventType x) 
       pure x

eventIDs :: EventCategory -> Array Int
eventIDs AccountLogon      = accountLogon
eventIDs AccountManagement = accountManagement
eventIDs DirectoryService  = directoryService
eventIDs LogonAndLogoff    = logonAndLogoff
eventIDs ObjectAccess      = objectAccess
eventIDs NonAudit          = nonAudit
eventIDs PolicyChange      = policyChange
eventIDs PrivilegeUse      = privilegeUse
eventIDs ProcessTracking   = processTracking
eventIDs System            = system
eventIDs Uncategorized     = uncategorized

eventCategories :: Array EventCategory
eventCategories = 
  [ AccountLogon
  , AccountManagement
  , DirectoryService
  , LogonAndLogoff
  , ObjectAccess
  , NonAudit
  , PolicyChange
  , PrivilegeUse
  , ProcessTracking
  , System
  , Uncategorized
  ]

writeEventCategory :: EventCategory -> String
writeEventCategory AccountLogon      = "account-logon"
writeEventCategory AccountManagement = "account-management"
writeEventCategory DirectoryService  = "directory-service"
writeEventCategory LogonAndLogoff    = "logon-and-logoff"
writeEventCategory ObjectAccess      = "object-access"
writeEventCategory NonAudit          = "non-audit"
writeEventCategory PolicyChange      = "policy-change"
writeEventCategory PrivilegeUse      = "privilege-use"
writeEventCategory ProcessTracking   = "process-tracking"
writeEventCategory System            = "system"
writeEventCategory Uncategorized     = "uncategorized"

readEventCategory :: String -> Either Exception.Error EventCategory
readEventCategory "account-logon"      = Right AccountLogon
readEventCategory "account-management" = Right AccountManagement
readEventCategory "directory-service"  = Right DirectoryService
readEventCategory "logon-and-logoff"   = Right LogonAndLogoff
readEventCategory "object-access"      = Right ObjectAccess
readEventCategory "non-audit"          = Right NonAudit
readEventCategory "policy-change"      = Right PolicyChange
readEventCategory "privilege-use"      = Right PrivilegeUse
readEventCategory "process-tracking"   = Right ProcessTracking
readEventCategory "system"             = Right System
readEventCategory "uncategorized"      = Right Uncategorized
readEventCategory _                    = Left (Exception.error "Invalid input.")

writeEventType :: EventType -> String
writeEventType Success = "success"
writeEventType Failure = "failure"

accountLogon :: Array Int
accountLogon =
  [ 4768
  , 4769
  , 4770
  , 4771
  , 4772
  , 4773
  , 4774
  , 4775
  , 4776
  , 4777
  , 4820
  ]

accountManagement :: Array Int
accountManagement =
  [ 4720
  , 4722
  , 4723
  , 4724
  , 4725
  , 4726
  , 4727
  , 4728
  , 4729
  , 4730
  , 4731
  , 4732
  , 4733
  , 4734
  , 4735
  , 4737
  , 4738
  , 4739
  , 4740
  , 4741
  , 4742
  , 4743
  , 4744
  , 4745
  , 4746
  , 4747
  , 4748
  , 4749
  , 4750
  , 4751
  , 4752
  , 4753
  , 4754
  , 4755
  , 4756
  , 4757
  , 4758
  , 4759
  , 4760
  , 4761
  , 4762
  , 4763
  , 4764
  , 4765
  , 4766
  , 4767
  , 4780
  , 4781
  , 4782
  , 4783
  , 4784
  , 4785
  , 4786
  , 4787
  , 4788
  , 4789
  , 4790
  , 4791
  , 4792
  , 4793
  , 4794
  , 4797
  , 4798
  , 4799
  , 5376
  , 5377
  ]


uncategorized :: Array Int
uncategorized =
  [ 4864
  , 4909
  , 4910
  , 4953
  , 4960
  , 4961
  , 4962
  , 4963
  , 4965
  , 5039
  , 5040
  , 5041
  , 5042
  , 5043
  , 5044
  , 5045
  , 5046
  , 5047
  , 5048
  , 5049
  , 5050
  , 5051
  , 5057
  , 5060
  , 5062
  , 5121
  , 5122
  , 5123
  , 5124
  , 5125
  , 5126
  , 5127
  ]

system :: Array Int
system =
  [ 4608
  , 4609
  , 4610
  , 4611
  , 4612
  , 4614
  , 4615
  , 4616
  , 4618
  , 4621
  , 4622
  , 4697
  , 4821
  , 4822
  , 4823
  , 4824
  , 4825
  , 4830
  , 5024
  , 5025
  , 5027
  , 5028
  , 5029
  , 5030
  , 5032
  , 5033
  , 5034
  , 5035
  , 5037
  , 5038
  , 5056
  , 5058
  , 5059
  , 5061
  , 5071
  , 5146
  , 5147
  , 5379
  , 5380
  , 5381
  , 5382
  , 5478
  , 5479
  , 5480
  , 5483
  , 5484
  , 5485
  , 5890
  , 6281
  , 6400
  , 6401
  , 6402
  , 6403
  , 6404
  , 6405
  , 6406
  , 6407
  , 6408
  , 6409
  , 6410
  , 6417
  , 6418
  , 8191
  ]

directoryService :: Array Int
directoryService =
  [ 4661
  , 4662
  , 4928
  , 4929
  , 4930
  , 4931
  , 4932
  , 4933
  , 4934
  , 4935
  , 4936
  , 4937
  , 5136
  , 5137
  , 5138
  , 5139
  , 5141
  , 5169
  , 5170
  ]

logonAndLogoff :: Array Int
logonAndLogoff =
  [ 4624
  , 4625
  , 4626
  , 4627
  , 4634
  , 4646
  , 4647
  , 4648
  , 4649
  , 4650
  , 4651
  , 4652
  , 4653
  , 4654
  , 4655
  , 4672
  , 4675
  , 4778
  , 4779
  , 4800
  , 4801
  , 4802
  , 4803
  , 4964
  , 4976
  , 4977
  , 4978
  , 4979
  , 4980
  , 4981
  , 4982
  , 4983
  , 4984
  , 5378
  , 5451
  , 5452
  , 5453
  , 5632
  , 5633
  , 6272
  , 6273
  , 6274
  , 6275
  , 6276
  , 6277
  , 6278
  , 6279
  , 6280
  ]

nonAudit :: Array Int
nonAudit =
  [ 1100
  , 1101
  , 1102
  , 1104
  , 1105
  , 1108
  ]

objectAccess :: Array Int
objectAccess =
  [ 4656
  , 4657
  , 4658
  , 4659
  , 4660
  , 4661
  , 4663
  , 4664
  , 4665
  , 4666
  , 4667
  , 4668
  , 4670
  , 4671
  , 4690
  , 4691
  , 4698
  , 4699
  , 4700
  , 4701
  , 4702
  , 4818
  , 4868
  , 4869
  , 4870
  , 4871
  , 4872
  , 4873
  , 4874
  , 4875
  , 4876
  , 4877
  , 4878
  , 4879
  , 4880
  , 4881
  , 4882
  , 4883
  , 4884
  , 4885
  , 4886
  , 4887
  , 4888
  , 4889
  , 4890
  , 4891
  , 4892
  , 4893
  , 4894
  , 4895
  , 4896
  , 4897
  , 4898
  , 4899
  , 4900
  , 4985
  , 5031
  , 5120
  , 5140
  , 5142
  , 5143
  , 5144
  , 5145
  , 5148
  , 5149
  , 5150
  , 5151
  , 5152
  , 5153
  , 5154
  , 5155
  , 5156
  , 5157
  , 5158
  , 5159
  , 5168
  , 5888
  , 5889
  ]

policyChange :: Array Int
policyChange =
  [ 4670
  , 4703
  , 4704
  , 4705
  , 4706
  , 4707
  , 4709
  , 4710
  , 4711
  , 4712
  , 4713
  , 4714
  , 4715
  , 4716
  , 4717
  , 4718
  , 4719
  , 4817
  , 4819
  , 4826
  , 4865
  , 4866
  , 4867
  , 4902
  , 4904
  , 4905
  , 4906
  , 4907
  , 4908
  , 4911
  , 4912
  , 4913
  , 4944
  , 4945
  , 4946
  , 4947
  , 4948
  , 4949
  , 4950
  , 4951
  , 4952
  , 4954
  , 4956
  , 4957
  , 4958
  , 5063
  , 5064
  , 5065
  , 5066
  , 5067
  , 5068
  , 5069
  , 5070
  , 5440
  , 5441
  , 5442
  , 5443
  , 5444
  , 5446
  , 5447
  , 5448
  , 5449
  , 5450
  , 5456
  , 5457
  , 5458
  , 5459
  , 5460
  , 5461
  , 5462
  , 5463
  , 5464
  , 5465
  , 5466
  , 5467
  , 5468
  , 5471
  , 5472
  , 5473
  , 5474
  , 5477
  , 6144
  , 6145
  ]

processTracking :: Array Int
processTracking =
  [ 4688
  , 4689
  , 4692
  , 4693
  , 4694
  , 4695
  , 4696
  , 4816
  , 5712
  , 6416
  , 6419
  , 6420
  , 6421
  , 6422
  , 6423
  , 6424
  ]

privilegeUse :: Array Int
privilegeUse = [ 4673, 4674 ]

