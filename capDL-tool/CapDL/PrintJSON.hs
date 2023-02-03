{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CapDL.PrintJSON
    ( printJSON
    ) where

import Control.Exception (assert)
import Data.Aeson (Key, ToJSON, Value(String), encode, toJSON, (.=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Map as M

import CapDL.PrintUtils (sortObjects)
import qualified CapDL.Model as C

---

printJSON :: C.ObjectSizeMap -> C.Model Word -> String
printJSON = curry $ unpack . encode . uncurry render

---

type Badge = Word
type CPtr = Word

type ObjID = Integer
type CapSlot = Integer
type CapTable = [(CapSlot, Cap)]

data Spec = Spec
    { objects :: [NamedObject]
    , irqs :: [(Word, ObjID)]
    , asid_slots :: [ObjID]
    } deriving (Eq, Show, Generic, ToJSON)

data NamedObject = NamedObject
    { name :: String
    , object :: Object
    } deriving (Eq, Show, Generic, ToJSON)

data Object =
      Object_Untyped ObjectUntyped
    | Object_Endpoint
    | Object_Notification
    | Object_CNode ObjectCNode
    | Object_TCB ObjectTCB
    | Object_Irq ObjectIrq
    | Object_VCPU
    | Object_SmallPage ObjectSmallPage
    | Object_LargePage ObjectLargePage
    | Object_PT ObjectPT
    | Object_PD ObjectPD
    | Object_PUD ObjectPUD
    | Object_PGD ObjectPGD
    | Object_ASIDPool ObjectASIDPool
    | Object_ARMIrq ObjectARMIrq
    deriving (Eq, Show)

instance ToJSON Object where
    toJSON obj = case obj of
        Object_Untyped obj -> tagged "Untyped" obj
        Object_Endpoint -> String "Endpoint"
        Object_Notification -> String "Notification"
        Object_CNode obj -> tagged "CNode" obj
        Object_TCB obj -> tagged "TCB" obj
        Object_Irq obj -> tagged "Irq" obj
        Object_VCPU -> String "VCPU"
        Object_SmallPage obj -> tagged "SmallPage" obj
        Object_LargePage obj -> tagged "LargePage" obj
        Object_PT obj -> tagged "PT" obj
        Object_PD obj -> tagged "PD" obj
        Object_PUD obj -> tagged "PUD" obj
        Object_PGD obj -> tagged "PGD" obj
        Object_ASIDPool obj -> tagged "ASIDPool" obj
        Object_ARMIrq obj -> tagged "ARMIrq" obj

data Cap =
      Cap_Untyped CapUntyped
    | Cap_Endpoint CapEndpoint
    | Cap_Notification CapNotification
    | Cap_CNode CapCNode
    | Cap_TCB CapTCB
    | Cap_IrqHandler CapIrqHandler
    | Cap_VCPU CapVCPU
    | Cap_SmallPage CapSmallPage
    | Cap_LargePage CapLargePage
    | Cap_PT CapPT
    | Cap_PD CapPD
    | Cap_PUD CapPUD
    | Cap_PGD CapPGD
    | Cap_ASIDPool CapASIDPool
    | Cap_ARMIrqHandler CapARMIrqHandler
    deriving (Eq, Show)

instance ToJSON Cap where
    toJSON cap' = case cap' of
        Cap_Untyped cap -> tagged "Untyped" cap
        Cap_Endpoint cap -> tagged "Endpoint" cap
        Cap_Notification cap -> tagged "Notification" cap
        Cap_CNode cap -> tagged "CNode" cap
        Cap_TCB cap -> tagged "TCB" cap
        Cap_IrqHandler cap -> tagged "IrqHandler" cap
        Cap_VCPU cap -> tagged "VCPU" cap
        Cap_SmallPage cap -> tagged "SmallPage" cap
        Cap_LargePage cap -> tagged "LargePage" cap
        Cap_PT cap -> tagged "PT" cap
        Cap_PD cap -> tagged "PD" cap
        Cap_PUD cap -> tagged "PUD" cap
        Cap_PGD cap -> tagged "PGD" cap
        Cap_ASIDPool cap -> tagged "ASIDPool" cap
        Cap_ARMIrqHandler cap -> tagged "ARMIrqHandler" cap

data Rights = Rights
    { rights_read :: Bool
    , rights_write :: Bool
    , rights_grant :: Bool
    , rights_grant_reply :: Bool
    } deriving (Eq, Show)

-- HACK until NoFieldSelectors is available
instance ToJSON Rights where
    toJSON Rights {..} = Aeson.object
        [ "read" .= rights_read
        , "write" .= rights_write
        , "grant" .= rights_grant
        , "grant_reply" .= rights_grant_reply
        ]

emptyRights :: Rights
emptyRights = Rights False False False False

type Fill = [FillEntry]

data FillEntry = FillEntry
    { range :: FillEntryRange
    , content :: FillEntryContent
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryRange = FillEntryRange
    { start :: Word
    , end :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryContent =
      FillEntryContent_Data FillEntryContentFile
    | FillEntryContent_BootInfo FillEntryContentBootInfo
    deriving (Eq, Show)

instance ToJSON FillEntryContent where
    toJSON content = case content of
        FillEntryContent_Data file -> tagged "Data" file
        FillEntryContent_BootInfo bootinfo -> tagged "BootInfo" bootinfo

data FillEntryContentFile = FillEntryContentFile
    { file :: String
    , file_offset :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryContentBootInfo = FillEntryContentBootInfo
    { id :: FillEntryContentBootInfoId
    , offset :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data FillEntryContentBootInfoId =
      Padding
    | Fdt
    deriving (Eq, Show, Generic, ToJSON)

data ObjectUntyped = ObjectUntyped
    { size_bits :: Word
    , paddr :: Maybe Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectCNode = ObjectCNode
    { size_bits :: Word
    , slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectTCB = ObjectTCB
    { slots :: CapTable
    , fault_ep :: CPtr
    , init_args :: [Maybe Word] -- invariant: length init_args == 4
    , extra_info :: ObjectTCBExtraInfo
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectTCBExtraInfo = ObjectTCBExtraInfo
    { affinity :: Word
    , prio :: Word
    , max_prio :: Word
    , resume :: Bool
    , ip :: Word
    , sp :: Word
    , spsr :: Word
    , ipc_buffer_addr :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectIrq = ObjectIrq
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectSmallPage = ObjectSmallPage
    { paddr :: Maybe Word
    , fill :: Fill
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectLargePage = ObjectLargePage
    { paddr :: Maybe Word
    , fill :: Fill
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectPT = ObjectPT
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectPD = ObjectPD
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectPUD = ObjectPUD
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectPGD = ObjectPGD
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectASIDPool = ObjectASIDPool
    { high :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data ObjectARMIrq = ObjectARMIrq
    { slots :: CapTable
    , trigger :: Word
    , target :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data CapUntyped = CapUntyped
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapEndpoint = CapEndpoint
    { object :: ObjID
    , badge :: Badge
    , rights :: Rights
    } deriving (Eq, Show, Generic, ToJSON)

data CapNotification = CapNotification
    { object :: ObjID
    , badge :: Badge
    , rights :: Rights
    } deriving (Eq, Show, Generic, ToJSON)

data CapCNode = CapCNode
    { object :: ObjID
    , guard :: Word
    , guard_size :: Word
    } deriving (Eq, Show, Generic, ToJSON)

data CapTCB = CapTCB
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapIrqHandler = CapIrqHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapVCPU = CapVCPU
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapSmallPage = CapSmallPage
    { object :: ObjID
    , rights :: Rights
    , cached :: Bool
    } deriving (Eq, Show, Generic, ToJSON)

data CapLargePage = CapLargePage
    { object :: ObjID
    , rights :: Rights
    , cached :: Bool
    } deriving (Eq, Show, Generic, ToJSON)

data CapPT = CapPT
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapPD = CapPD
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapPUD = CapPUD
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapPGD = CapPGD
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapASIDPool = CapASIDPool
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

data CapARMIrqHandler = CapARMIrqHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON)

tagged :: ToJSON a => Key -> a -> Value
tagged tag value = Aeson.object [ tag .= toJSON value ]

---

render :: C.ObjectSizeMap -> C.Model Word -> Spec
render objSizeMap (C.Model _ objMap irqNode _ _) = Spec
    { objects
    , irqs
    , asid_slots = asidSlots
    }
  where
    sortedObjects = sortObjects objSizeMap (M.toList objMap)
    objIdToSeqId = M.fromList . flip zip [0..] $ map fst sortedObjects
    translateId = (M.!) objIdToSeqId
    translateCapTable = M.toList . M.map renderCap . M.mapKeys toInteger

    irqs =
        [ (irq, translateId obj)
        | (irq, obj) <- M.toAscList irqNode
        ] 

    asidSlots = assert (map fst table `isPrefixOf` [1..]) (map snd table)
      where
        table = sortBy (comparing fst)
            [ let Just asidHigh = maybeAsidHigh
              in assert (M.null lowSlots) (asidHigh, translateId objID)
            | (objID, C.ASIDPool lowSlots maybeAsidHigh) <- M.toList objMap
            ]

    objects =
        [ NamedObject
            { name = renderName name
            , object = renderObj value
            }
        | (name, value) <- sortedObjects
        ]

    renderObj object = case object of
        C.Untyped { maybeSizeBits = Just sizeBits, maybePaddr } -> Object_Untyped (ObjectUntyped sizeBits maybePaddr)
        C.Endpoint -> Object_Endpoint
        C.Notification -> Object_Notification
        C.Frame { vmSizeBits, maybePaddr, maybeFill } ->
            let fill = translateFill maybeFill
            in case vmSizeBits of
                12 -> Object_SmallPage (ObjectSmallPage maybePaddr fill)
                21 -> Object_LargePage (ObjectLargePage maybePaddr fill)
        C.PT slots -> Object_PT (ObjectPT (translateCapTable slots))
        C.PD slots -> Object_PD (ObjectPD (translateCapTable slots))
        C.PUD slots -> Object_PUD (ObjectPUD (translateCapTable slots))
        C.PGD slots -> Object_PGD (ObjectPGD (translateCapTable slots))
        C.CNode slots 0 -> Object_Irq (ObjectIrq (translateCapTable slots)) -- model uses 0-sized CNodes as token objects for IRQs
        C.CNode slots sizeBits -> Object_CNode (ObjectCNode sizeBits (translateCapTable slots))
        C.VCPU -> Object_VCPU
        C.ARMIrq slots trigger target -> Object_ARMIrq (ObjectARMIrq (translateCapTable slots) trigger target)
        C.ASIDPool slots (Just asidHigh) -> assert (M.null slots) Object_ASIDPool (ObjectASIDPool asidHigh)
        C.TCB
            { slots
            , faultEndpoint
            , extraInfo = Just extraInfo
            , initArguments
            } ->
            let C.TCBExtraInfo
                    { ipcBufferAddr
                    , ip = Just ip
                    , sp = Just sp
                    , spsr = Just spsr
                    , prio = Just prio
                    , max_prio = Just max_prio
                    , affin = Just affinity
                    , resume
                    } = extraInfo
            in Object_TCB (ObjectTCB
                { slots = translateCapTable slots
                , fault_ep = fromMaybe 0 faultEndpoint
                , init_args = assert (length initArguments <= 4) $ take 4 (map Just initArguments <>  repeat Nothing)
                , extra_info = ObjectTCBExtraInfo
                    { ipc_buffer_addr = ipcBufferAddr
                    , affinity = fromIntegral affinity
                    , prio = fromIntegral prio
                    , max_prio = fromIntegral max_prio
                    , resume = fromMaybe True resume
                    , ip
                    , sp
                    , spsr
                    }
                })
        x -> traceShow x undefined

    renderCap cap = case cap of
        C.UntypedCap capObj -> Cap_Untyped (CapUntyped (translateId capObj))
        C.EndpointCap capObj capBadge capRights -> Cap_Endpoint (CapEndpoint (translateId capObj) capBadge (translateRights capRights))
        C.NotificationCap capObj capBadge capRights -> Cap_Notification (CapNotification (translateId capObj) capBadge (translateRights capRights))
        C.CNodeCap capObj capGuard capGuardSize -> Cap_CNode (CapCNode (translateId capObj) capGuard capGuardSize)
        C.TCBCap capObj -> Cap_TCB (CapTCB (translateId capObj))
        C.IRQHandlerCap capObj -> Cap_IrqHandler (CapIrqHandler (translateId capObj))
        C.VCPUCap capObj -> Cap_VCPU (CapVCPU (translateId capObj))
        C.FrameCap { capObj, capRights, capCached } ->
            let Just (C.Frame { vmSizeBits }) = M.lookup capObj objMap
                id = translateId capObj
                rights = translateRights capRights
            in case vmSizeBits of
                12 -> Cap_SmallPage (CapSmallPage id rights capCached)
                21 -> Cap_LargePage (CapLargePage id rights capCached)
        C.PTCap capObj _ -> Cap_PT (CapPT (translateId capObj))
        C.PDCap capObj _ -> Cap_PD (CapPD (translateId capObj))
        C.PUDCap capObj _ -> Cap_PUD (CapPUD (translateId capObj))
        C.PGDCap capObj _ -> Cap_PGD (CapPGD (translateId capObj))
        C.ARMIRQHandlerCap capObj -> Cap_ARMIrqHandler (CapARMIrqHandler (translateId capObj))
        C.ASIDPoolCap capObj -> Cap_ASIDPool (CapASIDPool (translateId capObj))
        x -> traceShow x undefined

renderName :: C.ObjID -> String
renderName (name, Nothing) = name

translateFill :: Maybe [[String]] -> Fill
translateFill = map f . concat . toList
  where
    f (dest_offset:dest_len:rest) = FillEntry
        { range = FillEntryRange { start = start, end = end }
        , content
        }
      where
        start = read dest_offset
        len = read dest_len
        end = start + len
        content = case rest of
            "CDL_FrameFill_FileData":file:file_offset:[] -> FillEntryContent_Data
                (FillEntryContentFile
                    { file = tail (init file)
                    , file_offset = read file_offset
                    })
            "CDL_FrameFill_BootInfo":id:offset:[] -> FillEntryContent_BootInfo
                (FillEntryContentBootInfo
                    { id = case id of
                        "CDL_FrameFill_BootInfo_FDT" -> Fdt
                    , offset = read offset
                    })

translateRights :: C.CapRights -> Rights
translateRights = foldr f emptyRights
    where
    f right acc = case right of
        C.Read -> acc { rights_read = True }
        C.Write -> acc { rights_write = True }
        C.Grant -> acc { rights_grant = True }
        C.GrantReply -> acc { rights_grant_reply = True }
