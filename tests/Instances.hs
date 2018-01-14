{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import DockerEngine.Model
import DockerEngine.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary AuthConfig where
  arbitrary =
    AuthConfig
      <$> arbitrary -- authConfigUsername :: Maybe Text
      <*> arbitrary -- authConfigPassword :: Maybe Text
      <*> arbitrary -- authConfigEmail :: Maybe Text
      <*> arbitrary -- authConfigServeraddress :: Maybe Text
    
instance Arbitrary Body where
  arbitrary =
    Body
      <$> arbitrary -- bodyName :: Maybe Text
      <*> arbitrary -- bodyDescription :: Maybe Text
      <*> arbitrary -- bodyValue :: Maybe [Text]
    
instance Arbitrary Body1 where
  arbitrary =
    Body1
      <$> arbitrary -- body1ListenAddr :: Maybe Text
      <*> arbitrary -- body1AdvertiseAddr :: Maybe Text
      <*> arbitrary -- body1ForceNewCluster :: Maybe Bool
      <*> arbitrary -- body1Spec :: Maybe SwarmSpec
    
instance Arbitrary Body2 where
  arbitrary =
    Body2
      <$> arbitrary -- body2ListenAddr :: Maybe Text
      <*> arbitrary -- body2AdvertiseAddr :: Maybe Text
      <*> arbitrary -- body2RemoteAddrs :: Maybe Text
      <*> arbitrary -- body2JoinToken :: Maybe Text
    
instance Arbitrary Body3 where
  arbitrary =
    Body3
      <$> arbitrary -- body3UnlockKey :: Maybe Text
    
instance Arbitrary BuildInfo where
  arbitrary =
    BuildInfo
      <$> arbitrary -- buildInfoId :: Maybe Text
      <*> arbitrary -- buildInfoStream :: Maybe Text
      <*> arbitrary -- buildInfoError :: Maybe Text
      <*> arbitrary -- buildInfoErrorDetail :: Maybe ErrorDetail
      <*> arbitrary -- buildInfoStatus :: Maybe Text
      <*> arbitrary -- buildInfoProgress :: Maybe Text
      <*> arbitrary -- buildInfoProgressDetail :: Maybe ProgressDetail
    
instance Arbitrary ClusterInfo where
  arbitrary =
    ClusterInfo
      <$> arbitrary -- clusterInfoId :: Maybe Text
      <*> arbitrary -- clusterInfoVersion :: Maybe NodeVersion
      <*> arbitrary -- clusterInfoCreatedAt :: Maybe Text
      <*> arbitrary -- clusterInfoUpdatedAt :: Maybe Text
      <*> arbitrary -- clusterInfoSpec :: Maybe SwarmSpec
    
instance Arbitrary Config where
  arbitrary =
    Config
      <$> arbitrary -- configHostname :: Maybe Text
      <*> arbitrary -- configDomainname :: Maybe Text
      <*> arbitrary -- configUser :: Maybe Text
      <*> arbitrary -- configAttachStdin :: Maybe Bool
      <*> arbitrary -- configAttachStdout :: Maybe Bool
      <*> arbitrary -- configAttachStderr :: Maybe Bool
      <*> arbitrary -- configExposedPorts :: Maybe (Map.Map String A.Value)
      <*> arbitrary -- configTty :: Maybe Bool
      <*> arbitrary -- configOpenStdin :: Maybe Bool
      <*> arbitrary -- configStdinOnce :: Maybe Bool
      <*> arbitrary -- configEnv :: Maybe [Text]
      <*> arbitrary -- configHealthcheck :: Maybe ConfigHealthcheck
      <*> arbitrary -- configArgsEscaped :: Maybe Bool
      <*> arbitrary -- configImage :: Maybe Text
      <*> arbitrary -- configVolumes :: Maybe ConfigVolumes
      <*> arbitrary -- configWorkingDir :: Maybe Text
      <*> arbitrary -- configNetworkDisabled :: Maybe Bool
      <*> arbitrary -- configMacAddress :: Maybe Text
      <*> arbitrary -- configOnBuild :: Maybe [Text]
      <*> arbitrary -- configLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- configStopSignal :: Maybe Text
      <*> arbitrary -- configStopTimeout :: Maybe Int
      <*> arbitrary -- configShell :: Maybe [Text]
    
instance Arbitrary ConfigHealthcheck where
  arbitrary =
    ConfigHealthcheck
      <$> arbitrary -- configHealthcheckTest :: Maybe [Text]
      <*> arbitrary -- configHealthcheckInterval :: Maybe Int
      <*> arbitrary -- configHealthcheckTimeout :: Maybe Int
      <*> arbitrary -- configHealthcheckRetries :: Maybe Int
    
instance Arbitrary ConfigVolumes where
  arbitrary =
    ConfigVolumes
      <$> arbitrary -- configVolumesAdditionalProperties :: Maybe A.Value
    
instance Arbitrary Container where
  arbitrary =
    Container
      <$> arbitrary -- containerContainer :: Maybe Text
      <*> arbitrary -- containerEndpointConfig :: Maybe EndpointSettings
    
instance Arbitrary Container1 where
  arbitrary =
    Container1
      <$> arbitrary -- container1Container :: Maybe Text
      <*> arbitrary -- container1Force :: Maybe Bool
    
instance Arbitrary ContainerSummary where
  arbitrary =
    
    pure ContainerSummary
     
instance Arbitrary ContainerSummaryInner where
  arbitrary =
    ContainerSummaryInner
      <$> arbitrary -- containerSummaryInnerId :: Maybe Text
      <*> arbitrary -- containerSummaryInnerNames :: Maybe [Text]
      <*> arbitrary -- containerSummaryInnerImage :: Maybe Text
      <*> arbitrary -- containerSummaryInnerImageId :: Maybe Text
      <*> arbitrary -- containerSummaryInnerCommand :: Maybe Text
      <*> arbitrary -- containerSummaryInnerCreated :: Maybe Integer
      <*> arbitrary -- containerSummaryInnerPorts :: Maybe [Port]
      <*> arbitrary -- containerSummaryInnerSizeRw :: Maybe Integer
      <*> arbitrary -- containerSummaryInnerSizeRootFs :: Maybe Integer
      <*> arbitrary -- containerSummaryInnerLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- containerSummaryInnerState :: Maybe Text
      <*> arbitrary -- containerSummaryInnerStatus :: Maybe Text
      <*> arbitrary -- containerSummaryInnerHostConfig :: Maybe ContainerSummaryInnerHostConfig
      <*> arbitrary -- containerSummaryInnerNetworkSettings :: Maybe ContainerSummaryInnerNetworkSettings
      <*> arbitrary -- containerSummaryInnerMounts :: Maybe [Mount]
    
instance Arbitrary ContainerSummaryInnerHostConfig where
  arbitrary =
    ContainerSummaryInnerHostConfig
      <$> arbitrary -- containerSummaryInnerHostConfigNetworkMode :: Maybe Text
    
instance Arbitrary ContainerSummaryInnerNetworkSettings where
  arbitrary =
    ContainerSummaryInnerNetworkSettings
      <$> arbitrary -- containerSummaryInnerNetworkSettingsNetworks :: Maybe (Map.Map String EndpointSettings)
    
instance Arbitrary CreateImageInfo where
  arbitrary =
    CreateImageInfo
      <$> arbitrary -- createImageInfoError :: Maybe Text
      <*> arbitrary -- createImageInfoStatus :: Maybe Text
      <*> arbitrary -- createImageInfoProgress :: Maybe Text
      <*> arbitrary -- createImageInfoProgressDetail :: Maybe ProgressDetail
    
instance Arbitrary DeviceMapping where
  arbitrary =
    DeviceMapping
      <$> arbitrary -- deviceMappingPathOnHost :: Maybe Text
      <*> arbitrary -- deviceMappingPathInContainer :: Maybe Text
      <*> arbitrary -- deviceMappingCgroupPermissions :: Maybe Text
    
instance Arbitrary EndpointPortConfig where
  arbitrary =
    EndpointPortConfig
      <$> arbitrary -- endpointPortConfigName :: Maybe Text
      <*> arbitrary -- endpointPortConfigProtocol :: Maybe Text
      <*> arbitrary -- endpointPortConfigTargetPort :: Maybe Int
      <*> arbitrary -- endpointPortConfigPublishedPort :: Maybe Int
    
instance Arbitrary EndpointSettings where
  arbitrary =
    EndpointSettings
      <$> arbitrary -- endpointSettingsIpamConfig :: Maybe EndpointSettingsIPAMConfig
      <*> arbitrary -- endpointSettingsLinks :: Maybe [Text]
      <*> arbitrary -- endpointSettingsAliases :: Maybe [Text]
      <*> arbitrary -- endpointSettingsNetworkId :: Maybe Text
      <*> arbitrary -- endpointSettingsEndpointId :: Maybe Text
      <*> arbitrary -- endpointSettingsGateway :: Maybe Text
      <*> arbitrary -- endpointSettingsIpAddress :: Maybe Text
      <*> arbitrary -- endpointSettingsIpPrefixLen :: Maybe Int
      <*> arbitrary -- endpointSettingsIPv6Gateway :: Maybe Text
      <*> arbitrary -- endpointSettingsGlobalIPv6Address :: Maybe Text
      <*> arbitrary -- endpointSettingsGlobalIPv6PrefixLen :: Maybe Integer
      <*> arbitrary -- endpointSettingsMacAddress :: Maybe Text
    
instance Arbitrary EndpointSettingsIPAMConfig where
  arbitrary =
    EndpointSettingsIPAMConfig
      <$> arbitrary -- endpointSettingsIPAMConfigIPv4Address :: Maybe Text
      <*> arbitrary -- endpointSettingsIPAMConfigIPv6Address :: Maybe Text
      <*> arbitrary -- endpointSettingsIPAMConfigLinkLocalIPs :: Maybe [Text]
    
instance Arbitrary EndpointSpec where
  arbitrary =
    EndpointSpec
      <$> arbitrary -- endpointSpecMode :: Maybe Text
      <*> arbitrary -- endpointSpecPorts :: Maybe [EndpointPortConfig]
    
instance Arbitrary ErrorDetail where
  arbitrary =
    ErrorDetail
      <$> arbitrary -- errorDetailCode :: Maybe Int
      <*> arbitrary -- errorDetailMessage :: Maybe Text
    
instance Arbitrary ErrorResponse where
  arbitrary =
    ErrorResponse
      <$> arbitrary -- errorResponseMessage :: Text
    
instance Arbitrary ExecConfig where
  arbitrary =
    ExecConfig
      <$> arbitrary -- execConfigAttachStdin :: Maybe Bool
      <*> arbitrary -- execConfigAttachStdout :: Maybe Bool
      <*> arbitrary -- execConfigAttachStderr :: Maybe Bool
      <*> arbitrary -- execConfigDetachKeys :: Maybe Text
      <*> arbitrary -- execConfigTty :: Maybe Bool
      <*> arbitrary -- execConfigEnv :: Maybe [Text]
      <*> arbitrary -- execConfigCmd :: Maybe [Text]
      <*> arbitrary -- execConfigPrivileged :: Maybe Bool
      <*> arbitrary -- execConfigUser :: Maybe Text
    
instance Arbitrary ExecStartConfig where
  arbitrary =
    ExecStartConfig
      <$> arbitrary -- execStartConfigDetach :: Maybe Bool
      <*> arbitrary -- execStartConfigTty :: Maybe Bool
    
instance Arbitrary GraphDriver where
  arbitrary =
    GraphDriver
      <$> arbitrary -- graphDriverName :: Maybe Text
      <*> arbitrary -- graphDriverData :: Maybe (Map.Map String Text)
    
instance Arbitrary HostConfigLogConfig where
  arbitrary =
    HostConfigLogConfig
      <$> arbitrary -- hostConfigLogConfigType :: Maybe Text
      <*> arbitrary -- hostConfigLogConfigConfig :: Maybe (Map.Map String Text)
    
instance Arbitrary HostConfigPortBindings where
  arbitrary =
    HostConfigPortBindings
      <$> arbitrary -- hostConfigPortBindingsHostIp :: Maybe Text
      <*> arbitrary -- hostConfigPortBindingsHostPort :: Maybe Text
    
instance Arbitrary IPAM where
  arbitrary =
    IPAM
      <$> arbitrary -- iPAMDriver :: Maybe Text
      <*> arbitrary -- iPAMConfig :: Maybe [(Map.Map String Text)]
      <*> arbitrary -- iPAMOptions :: Maybe [(Map.Map String Text)]
    
instance Arbitrary IdResponse where
  arbitrary =
    IdResponse
      <$> arbitrary -- idResponseId :: Text
    
instance Arbitrary Image where
  arbitrary =
    Image
      <$> arbitrary -- imageId :: Maybe Text
      <*> arbitrary -- imageRepoTags :: Maybe [Text]
      <*> arbitrary -- imageRepoDigests :: Maybe [Text]
      <*> arbitrary -- imageParent :: Maybe Text
      <*> arbitrary -- imageComment :: Maybe Text
      <*> arbitrary -- imageCreated :: Maybe Text
      <*> arbitrary -- imageContainer :: Maybe Text
      <*> arbitrary -- imageContainerConfig :: Maybe Config
      <*> arbitrary -- imageDockerVersion :: Maybe Text
      <*> arbitrary -- imageAuthor :: Maybe Text
      <*> arbitrary -- imageConfig :: Maybe Config
      <*> arbitrary -- imageArchitecture :: Maybe Text
      <*> arbitrary -- imageOs :: Maybe Text
      <*> arbitrary -- imageSize :: Maybe Integer
      <*> arbitrary -- imageVirtualSize :: Maybe Integer
      <*> arbitrary -- imageGraphDriver :: Maybe GraphDriver
      <*> arbitrary -- imageRootFs :: Maybe ImageRootFS
    
instance Arbitrary ImageDeleteResponse where
  arbitrary =
    ImageDeleteResponse
      <$> arbitrary -- imageDeleteResponseUntagged :: Maybe Text
      <*> arbitrary -- imageDeleteResponseDeleted :: Maybe Text
    
instance Arbitrary ImageRootFS where
  arbitrary =
    ImageRootFS
      <$> arbitrary -- imageRootFSType :: Maybe Text
      <*> arbitrary -- imageRootFSLayers :: Maybe [Text]
      <*> arbitrary -- imageRootFSBaseLayer :: Maybe Text
    
instance Arbitrary ImageSummary where
  arbitrary =
    ImageSummary
      <$> arbitrary -- imageSummaryId :: Text
      <*> arbitrary -- imageSummaryParentId :: Text
      <*> arbitrary -- imageSummaryRepoTags :: [Text]
      <*> arbitrary -- imageSummaryRepoDigests :: [Text]
      <*> arbitrary -- imageSummaryCreated :: Int
      <*> arbitrary -- imageSummarySize :: Int
      <*> arbitrary -- imageSummarySharedSize :: Int
      <*> arbitrary -- imageSummaryVirtualSize :: Int
      <*> arbitrary -- imageSummaryLabels :: (Map.Map String Text)
      <*> arbitrary -- imageSummaryContainers :: Int
    
instance Arbitrary InlineResponse200 where
  arbitrary =
    InlineResponse200
      <$> arbitrary -- inlineResponse200Id :: Maybe Text
      <*> arbitrary -- inlineResponse200Created :: Maybe Text
      <*> arbitrary -- inlineResponse200Path :: Maybe Text
      <*> arbitrary -- inlineResponse200Args :: Maybe [Text]
      <*> arbitrary -- inlineResponse200State :: Maybe InlineResponse200State
      <*> arbitrary -- inlineResponse200Image :: Maybe Text
      <*> arbitrary -- inlineResponse200ResolvConfPath :: Maybe Text
      <*> arbitrary -- inlineResponse200HostnamePath :: Maybe Text
      <*> arbitrary -- inlineResponse200HostsPath :: Maybe Text
      <*> arbitrary -- inlineResponse200LogPath :: Maybe Text
      <*> arbitrary -- inlineResponse200Node :: Maybe A.Value
      <*> arbitrary -- inlineResponse200Name :: Maybe Text
      <*> arbitrary -- inlineResponse200RestartCount :: Maybe Int
      <*> arbitrary -- inlineResponse200Driver :: Maybe Text
      <*> arbitrary -- inlineResponse200MountLabel :: Maybe Text
      <*> arbitrary -- inlineResponse200ProcessLabel :: Maybe Text
      <*> arbitrary -- inlineResponse200AppArmorProfile :: Maybe Text
      <*> arbitrary -- inlineResponse200ExecIDs :: Maybe Text
      <*> arbitrary -- inlineResponse200HostConfig :: Maybe HostConfig
      <*> arbitrary -- inlineResponse200GraphDriver :: Maybe GraphDriver
      <*> arbitrary -- inlineResponse200SizeRw :: Maybe Integer
      <*> arbitrary -- inlineResponse200SizeRootFs :: Maybe Integer
      <*> arbitrary -- inlineResponse200Mounts :: Maybe [MountPoint]
      <*> arbitrary -- inlineResponse200Config :: Maybe Config
      <*> arbitrary -- inlineResponse200NetworkSettings :: Maybe NetworkConfig
    
instance Arbitrary InlineResponse2001 where
  arbitrary =
    InlineResponse2001
      <$> arbitrary -- inlineResponse2001Titles :: Maybe [Text]
      <*> arbitrary -- inlineResponse2001Processes :: Maybe [[Text]]
    
instance Arbitrary InlineResponse20010 where
  arbitrary =
    InlineResponse20010
      <$> arbitrary -- inlineResponse20010Architecture :: Maybe Text
      <*> arbitrary -- inlineResponse20010Containers :: Maybe Int
      <*> arbitrary -- inlineResponse20010ContainersRunning :: Maybe Int
      <*> arbitrary -- inlineResponse20010ContainersStopped :: Maybe Int
      <*> arbitrary -- inlineResponse20010ContainersPaused :: Maybe Int
      <*> arbitrary -- inlineResponse20010CpuCfsPeriod :: Maybe Bool
      <*> arbitrary -- inlineResponse20010CpuCfsQuota :: Maybe Bool
      <*> arbitrary -- inlineResponse20010Debug :: Maybe Bool
      <*> arbitrary -- inlineResponse20010DiscoveryBackend :: Maybe Text
      <*> arbitrary -- inlineResponse20010DockerRootDir :: Maybe Text
      <*> arbitrary -- inlineResponse20010Driver :: Maybe Text
      <*> arbitrary -- inlineResponse20010DriverStatus :: Maybe [[Text]]
      <*> arbitrary -- inlineResponse20010SystemStatus :: Maybe [[Text]]
      <*> arbitrary -- inlineResponse20010Plugins :: Maybe InlineResponse20010Plugins
      <*> arbitrary -- inlineResponse20010ExperimentalBuild :: Maybe Bool
      <*> arbitrary -- inlineResponse20010HttpProxy :: Maybe Text
      <*> arbitrary -- inlineResponse20010HttpsProxy :: Maybe Text
      <*> arbitrary -- inlineResponse20010Id :: Maybe Text
      <*> arbitrary -- inlineResponse20010IPv4Forwarding :: Maybe Bool
      <*> arbitrary -- inlineResponse20010Images :: Maybe Int
      <*> arbitrary -- inlineResponse20010IndexServerAddress :: Maybe Text
      <*> arbitrary -- inlineResponse20010InitPath :: Maybe Text
      <*> arbitrary -- inlineResponse20010InitSha1 :: Maybe Text
      <*> arbitrary -- inlineResponse20010KernelVersion :: Maybe Text
      <*> arbitrary -- inlineResponse20010Labels :: Maybe [Text]
      <*> arbitrary -- inlineResponse20010MemTotal :: Maybe Int
      <*> arbitrary -- inlineResponse20010MemoryLimit :: Maybe Bool
      <*> arbitrary -- inlineResponse20010Ncpu :: Maybe Int
      <*> arbitrary -- inlineResponse20010NEventsListener :: Maybe Int
      <*> arbitrary -- inlineResponse20010NFd :: Maybe Int
      <*> arbitrary -- inlineResponse20010NGoroutines :: Maybe Int
      <*> arbitrary -- inlineResponse20010Name :: Maybe Text
      <*> arbitrary -- inlineResponse20010NoProxy :: Maybe Text
      <*> arbitrary -- inlineResponse20010OomKillDisable :: Maybe Bool
      <*> arbitrary -- inlineResponse20010OsType :: Maybe Text
      <*> arbitrary -- inlineResponse20010OomScoreAdj :: Maybe Int
      <*> arbitrary -- inlineResponse20010OperatingSystem :: Maybe Text
      <*> arbitrary -- inlineResponse20010RegistryConfig :: Maybe InlineResponse20010RegistryConfig
      <*> arbitrary -- inlineResponse20010SwapLimit :: Maybe Bool
      <*> arbitrary -- inlineResponse20010SystemTime :: Maybe Text
      <*> arbitrary -- inlineResponse20010ServerVersion :: Maybe Text
    
instance Arbitrary InlineResponse20010Plugins where
  arbitrary =
    InlineResponse20010Plugins
      <$> arbitrary -- inlineResponse20010PluginsVolume :: Maybe [Text]
      <*> arbitrary -- inlineResponse20010PluginsNetwork :: Maybe [Text]
    
instance Arbitrary InlineResponse20010RegistryConfig where
  arbitrary =
    InlineResponse20010RegistryConfig
      <$> arbitrary -- inlineResponse20010RegistryConfigIndexConfigs :: Maybe (Map.Map String InlineResponse20010RegistryConfigIndexConfigs)
      <*> arbitrary -- inlineResponse20010RegistryConfigInsecureRegistryCidRs :: Maybe [Text]
    
instance Arbitrary InlineResponse20010RegistryConfigIndexConfigs where
  arbitrary =
    InlineResponse20010RegistryConfigIndexConfigs
      <$> arbitrary -- inlineResponse20010RegistryConfigIndexConfigsMirrors :: Maybe [Text]
      <*> arbitrary -- inlineResponse20010RegistryConfigIndexConfigsName :: Maybe Text
      <*> arbitrary -- inlineResponse20010RegistryConfigIndexConfigsOfficial :: Maybe Bool
      <*> arbitrary -- inlineResponse20010RegistryConfigIndexConfigsSecure :: Maybe Bool
    
instance Arbitrary InlineResponse20011 where
  arbitrary =
    InlineResponse20011
      <$> arbitrary -- inlineResponse20011Version :: Maybe Text
      <*> arbitrary -- inlineResponse20011ApiVersion :: Maybe Text
      <*> arbitrary -- inlineResponse20011MinApiVersion :: Maybe Text
      <*> arbitrary -- inlineResponse20011GitCommit :: Maybe Text
      <*> arbitrary -- inlineResponse20011GoVersion :: Maybe Text
      <*> arbitrary -- inlineResponse20011Os :: Maybe Text
      <*> arbitrary -- inlineResponse20011Arch :: Maybe Text
      <*> arbitrary -- inlineResponse20011KernelVersion :: Maybe Text
      <*> arbitrary -- inlineResponse20011Experimental :: Maybe Bool
      <*> arbitrary -- inlineResponse20011BuildTime :: Maybe Text
    
instance Arbitrary InlineResponse20012 where
  arbitrary =
    InlineResponse20012
      <$> arbitrary -- inlineResponse20012Type :: Maybe Text
      <*> arbitrary -- inlineResponse20012Action :: Maybe Text
      <*> arbitrary -- inlineResponse20012Actor :: Maybe InlineResponse20012Actor
      <*> arbitrary -- inlineResponse20012Time :: Maybe Int
      <*> arbitrary -- inlineResponse20012TimeNano :: Maybe Integer
    
instance Arbitrary InlineResponse20012Actor where
  arbitrary =
    InlineResponse20012Actor
      <$> arbitrary -- inlineResponse20012ActorId :: Maybe Text
      <*> arbitrary -- inlineResponse20012ActorAttributes :: Maybe (Map.Map String Text)
    
instance Arbitrary InlineResponse20013 where
  arbitrary =
    InlineResponse20013
      <$> arbitrary -- inlineResponse20013LayersSize :: Maybe Integer
      <*> arbitrary -- inlineResponse20013Images :: Maybe [ImageSummary]
      <*> arbitrary -- inlineResponse20013Containers :: Maybe [ContainerSummary]
      <*> arbitrary -- inlineResponse20013Volumes :: Maybe [Volume]
    
instance Arbitrary InlineResponse20014 where
  arbitrary =
    InlineResponse20014
      <$> arbitrary -- inlineResponse20014Id :: Maybe Text
      <*> arbitrary -- inlineResponse20014Running :: Maybe Bool
      <*> arbitrary -- inlineResponse20014ExitCode :: Maybe Int
      <*> arbitrary -- inlineResponse20014ProcessConfig :: Maybe ProcessConfig
      <*> arbitrary -- inlineResponse20014OpenStdin :: Maybe Bool
      <*> arbitrary -- inlineResponse20014OpenStderr :: Maybe Bool
      <*> arbitrary -- inlineResponse20014OpenStdout :: Maybe Bool
      <*> arbitrary -- inlineResponse20014ContainerId :: Maybe Text
      <*> arbitrary -- inlineResponse20014Pid :: Maybe Int
    
instance Arbitrary InlineResponse20015 where
  arbitrary =
    InlineResponse20015
      <$> arbitrary -- inlineResponse20015Volumes :: [Volume]
      <*> arbitrary -- inlineResponse20015Warnings :: [Text]
    
instance Arbitrary InlineResponse20016 where
  arbitrary =
    InlineResponse20016
      <$> arbitrary -- inlineResponse20016VolumesDeleted :: Maybe [Text]
      <*> arbitrary -- inlineResponse20016SpaceReclaimed :: Maybe Integer
    
instance Arbitrary InlineResponse20017 where
  arbitrary =
    InlineResponse20017
      <$> arbitrary -- inlineResponse20017VolumesDeleted :: Maybe [Text]
    
instance Arbitrary InlineResponse20018 where
  arbitrary =
    InlineResponse20018
      <$> arbitrary -- inlineResponse20018Name :: Maybe Text
      <*> arbitrary -- inlineResponse20018Description :: Maybe Text
      <*> arbitrary -- inlineResponse20018Value :: Maybe [Text]
    
instance Arbitrary InlineResponse20019 where
  arbitrary =
    InlineResponse20019
      <$> arbitrary -- inlineResponse20019UnlockKey :: Maybe Text
    
instance Arbitrary InlineResponse2002 where
  arbitrary =
    InlineResponse2002
      <$> arbitrary -- inlineResponse2002Path :: Maybe Text
      <*> arbitrary -- inlineResponse2002Kind :: Maybe Int
    
instance Arbitrary InlineResponse2003 where
  arbitrary =
    InlineResponse2003
      <$> arbitrary -- inlineResponse2003Warnings :: Maybe [Text]
    
instance Arbitrary InlineResponse2004 where
  arbitrary =
    InlineResponse2004
      <$> arbitrary -- inlineResponse2004StatusCode :: Int
    
instance Arbitrary InlineResponse2005 where
  arbitrary =
    InlineResponse2005
      <$> arbitrary -- inlineResponse2005ContainersDeleted :: Maybe [Text]
      <*> arbitrary -- inlineResponse2005SpaceReclaimed :: Maybe Integer
    
instance Arbitrary InlineResponse2006 where
  arbitrary =
    InlineResponse2006
      <$> arbitrary -- inlineResponse2006Id :: Maybe Text
      <*> arbitrary -- inlineResponse2006Created :: Maybe Integer
      <*> arbitrary -- inlineResponse2006CreatedBy :: Maybe Text
      <*> arbitrary -- inlineResponse2006Tags :: Maybe [Text]
      <*> arbitrary -- inlineResponse2006Size :: Maybe Integer
      <*> arbitrary -- inlineResponse2006Comment :: Maybe Text
    
instance Arbitrary InlineResponse2007 where
  arbitrary =
    InlineResponse2007
      <$> arbitrary -- inlineResponse2007Description :: Maybe Text
      <*> arbitrary -- inlineResponse2007IsOfficial :: Maybe Bool
      <*> arbitrary -- inlineResponse2007IsAutomated :: Maybe Bool
      <*> arbitrary -- inlineResponse2007Name :: Maybe Text
      <*> arbitrary -- inlineResponse2007StarCount :: Maybe Int
    
instance Arbitrary InlineResponse2008 where
  arbitrary =
    InlineResponse2008
      <$> arbitrary -- inlineResponse2008ImagesDeleted :: Maybe [ImageDeleteResponse]
      <*> arbitrary -- inlineResponse2008SpaceReclaimed :: Maybe Integer
    
instance Arbitrary InlineResponse2009 where
  arbitrary =
    InlineResponse2009
      <$> arbitrary -- inlineResponse2009Status :: Text
      <*> arbitrary -- inlineResponse2009IdentityToken :: Maybe Text
    
instance Arbitrary InlineResponse200State where
  arbitrary =
    InlineResponse200State
      <$> arbitrary -- inlineResponse200StateStatus :: Maybe Text
      <*> arbitrary -- inlineResponse200StateRunning :: Maybe Bool
      <*> arbitrary -- inlineResponse200StatePaused :: Maybe Bool
      <*> arbitrary -- inlineResponse200StateRestarting :: Maybe Bool
      <*> arbitrary -- inlineResponse200StateOomKilled :: Maybe Bool
      <*> arbitrary -- inlineResponse200StateDead :: Maybe Bool
      <*> arbitrary -- inlineResponse200StatePid :: Maybe Int
      <*> arbitrary -- inlineResponse200StateExitCode :: Maybe Int
      <*> arbitrary -- inlineResponse200StateError :: Maybe Text
      <*> arbitrary -- inlineResponse200StateStartedAt :: Maybe Text
      <*> arbitrary -- inlineResponse200StateFinishedAt :: Maybe Text
    
instance Arbitrary InlineResponse201 where
  arbitrary =
    InlineResponse201
      <$> arbitrary -- inlineResponse201Id :: Text
      <*> arbitrary -- inlineResponse201Warnings :: [Text]
    
instance Arbitrary InlineResponse2011 where
  arbitrary =
    InlineResponse2011
      <$> arbitrary -- inlineResponse2011Id :: Maybe Text
      <*> arbitrary -- inlineResponse2011Warning :: Maybe Text
    
instance Arbitrary InlineResponse2012 where
  arbitrary =
    InlineResponse2012
      <$> arbitrary -- inlineResponse2012Id :: Maybe Text
      <*> arbitrary -- inlineResponse2012Warning :: Maybe Text
    
instance Arbitrary InlineResponse2013 where
  arbitrary =
    InlineResponse2013
      <$> arbitrary -- inlineResponse2013Id :: Maybe Text
    
instance Arbitrary Mount where
  arbitrary =
    Mount
      <$> arbitrary -- mountTarget :: Maybe Text
      <*> arbitrary -- mountType :: Maybe Text
      <*> arbitrary -- mountReadOnly :: Maybe Bool
      <*> arbitrary -- mountBindOptions :: Maybe A.Value
      <*> arbitrary -- mountVolumeOptions :: Maybe MountVolumeOptions
      <*> arbitrary -- mountTmpfsOptions :: Maybe MountTmpfsOptions
    
instance Arbitrary MountPoint where
  arbitrary =
    MountPoint
      <$> arbitrary -- mountPointType :: Maybe Text
      <*> arbitrary -- mountPointName :: Maybe Text
      <*> arbitrary -- mountPointSource :: Maybe Text
      <*> arbitrary -- mountPointDestination :: Maybe Text
      <*> arbitrary -- mountPointDriver :: Maybe Text
      <*> arbitrary -- mountPointMode :: Maybe Text
      <*> arbitrary -- mountPointRw :: Maybe Bool
      <*> arbitrary -- mountPointPropagation :: Maybe Text
    
instance Arbitrary MountTmpfsOptions where
  arbitrary =
    MountTmpfsOptions
      <$> arbitrary -- mountTmpfsOptionsSizeBytes :: Maybe Integer
      <*> arbitrary -- mountTmpfsOptionsMode :: Maybe Int
    
instance Arbitrary MountVolumeOptions where
  arbitrary =
    MountVolumeOptions
      <$> arbitrary -- mountVolumeOptionsNoCopy :: Maybe Bool
      <*> arbitrary -- mountVolumeOptionsLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- mountVolumeOptionsDriverConfig :: Maybe MountVolumeOptionsDriverConfig
    
instance Arbitrary MountVolumeOptionsDriverConfig where
  arbitrary =
    MountVolumeOptionsDriverConfig
      <$> arbitrary -- mountVolumeOptionsDriverConfigName :: Maybe Text
      <*> arbitrary -- mountVolumeOptionsDriverConfigOptions :: Maybe (Map.Map String Text)
    
instance Arbitrary Network where
  arbitrary =
    Network
      <$> arbitrary -- networkName :: Maybe Text
      <*> arbitrary -- networkId :: Maybe Text
      <*> arbitrary -- networkCreated :: Maybe Text
      <*> arbitrary -- networkScope :: Maybe Text
      <*> arbitrary -- networkDriver :: Maybe Text
      <*> arbitrary -- networkEnableIPv6 :: Maybe Bool
      <*> arbitrary -- networkIpam :: Maybe IPAM
      <*> arbitrary -- networkInternal :: Maybe Bool
      <*> arbitrary -- networkContainers :: Maybe (Map.Map String NetworkContainer)
      <*> arbitrary -- networkOptions :: Maybe (Map.Map String Text)
      <*> arbitrary -- networkLabels :: Maybe (Map.Map String Text)
    
instance Arbitrary NetworkConfig2 where
  arbitrary =
    NetworkConfig2
      <$> arbitrary -- networkConfig2Name :: Text
      <*> arbitrary -- networkConfig2CheckDuplicate :: Maybe Bool
      <*> arbitrary -- networkConfig2Driver :: Maybe Text
      <*> arbitrary -- networkConfig2Internal :: Maybe Bool
      <*> arbitrary -- networkConfig2Ipam :: Maybe IPAM
      <*> arbitrary -- networkConfig2EnableIPv6 :: Maybe Bool
      <*> arbitrary -- networkConfig2Options :: Maybe (Map.Map String Text)
      <*> arbitrary -- networkConfig2Labels :: Maybe (Map.Map String Text)
    
instance Arbitrary NetworkContainer where
  arbitrary =
    NetworkContainer
      <$> arbitrary -- networkContainerEndpointId :: Maybe Text
      <*> arbitrary -- networkContainerMacAddress :: Maybe Text
      <*> arbitrary -- networkContainerIPv4Address :: Maybe Text
      <*> arbitrary -- networkContainerIPv6Address :: Maybe Text
    
instance Arbitrary Node where
  arbitrary =
    Node
      <$> arbitrary -- nodeId :: Maybe Text
      <*> arbitrary -- nodeVersion :: Maybe NodeVersion
      <*> arbitrary -- nodeCreatedAt :: Maybe Text
      <*> arbitrary -- nodeUpdatedAt :: Maybe Text
      <*> arbitrary -- nodeSpec :: Maybe NodeSpec
      <*> arbitrary -- nodeDescription :: Maybe NodeDescription
    
instance Arbitrary NodeDescription where
  arbitrary =
    NodeDescription
      <$> arbitrary -- nodeDescriptionHostname :: Maybe Text
      <*> arbitrary -- nodeDescriptionPlatform :: Maybe NodeDescriptionPlatform
      <*> arbitrary -- nodeDescriptionResources :: Maybe NodeDescriptionResources
      <*> arbitrary -- nodeDescriptionEngine :: Maybe NodeDescriptionEngine
    
instance Arbitrary NodeDescriptionEngine where
  arbitrary =
    NodeDescriptionEngine
      <$> arbitrary -- nodeDescriptionEngineEngineVersion :: Maybe Text
      <*> arbitrary -- nodeDescriptionEngineLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- nodeDescriptionEnginePlugins :: Maybe [NodeDescriptionEnginePlugins]
    
instance Arbitrary NodeDescriptionEnginePlugins where
  arbitrary =
    NodeDescriptionEnginePlugins
      <$> arbitrary -- nodeDescriptionEnginePluginsType :: Maybe Text
      <*> arbitrary -- nodeDescriptionEnginePluginsName :: Maybe Text
    
instance Arbitrary NodeDescriptionPlatform where
  arbitrary =
    NodeDescriptionPlatform
      <$> arbitrary -- nodeDescriptionPlatformArchitecture :: Maybe Text
      <*> arbitrary -- nodeDescriptionPlatformOs :: Maybe Text
    
instance Arbitrary NodeDescriptionResources where
  arbitrary =
    NodeDescriptionResources
      <$> arbitrary -- nodeDescriptionResourcesNanoCpUs :: Maybe Integer
      <*> arbitrary -- nodeDescriptionResourcesMemoryBytes :: Maybe Integer
    
instance Arbitrary NodeSpec where
  arbitrary =
    NodeSpec
      <$> arbitrary -- nodeSpecName :: Maybe Text
      <*> arbitrary -- nodeSpecLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- nodeSpecRole :: Maybe Text
      <*> arbitrary -- nodeSpecAvailability :: Maybe Text
    
instance Arbitrary NodeVersion where
  arbitrary =
    NodeVersion
      <$> arbitrary -- nodeVersionIndex :: Maybe Integer
    
instance Arbitrary Plugin where
  arbitrary =
    Plugin
      <$> arbitrary -- pluginId :: Maybe Text
      <*> arbitrary -- pluginName :: Text
      <*> arbitrary -- pluginEnabled :: Bool
      <*> arbitrary -- pluginSettings :: PluginSettings
      <*> arbitrary -- pluginConfig :: PluginConfig
    
instance Arbitrary PluginConfig where
  arbitrary =
    PluginConfig
      <$> arbitrary -- pluginConfigDescription :: Text
      <*> arbitrary -- pluginConfigDocumentation :: Text
      <*> arbitrary -- pluginConfigInterface :: PluginConfigInterface
      <*> arbitrary -- pluginConfigEntrypoint :: [Text]
      <*> arbitrary -- pluginConfigWorkDir :: Text
      <*> arbitrary -- pluginConfigUser :: Maybe PluginConfigUser
      <*> arbitrary -- pluginConfigNetwork :: PluginConfigNetwork
      <*> arbitrary -- pluginConfigLinux :: PluginConfigLinux
      <*> arbitrary -- pluginConfigPropagatedMount :: Text
      <*> arbitrary -- pluginConfigMounts :: [PluginMount]
      <*> arbitrary -- pluginConfigEnv :: [PluginEnv]
      <*> arbitrary -- pluginConfigArgs :: PluginConfigArgs
      <*> arbitrary -- pluginConfigRootfs :: Maybe PluginConfigRootfs
    
instance Arbitrary PluginConfigArgs where
  arbitrary =
    PluginConfigArgs
      <$> arbitrary -- pluginConfigArgsName :: Text
      <*> arbitrary -- pluginConfigArgsDescription :: Text
      <*> arbitrary -- pluginConfigArgsSettable :: [Text]
      <*> arbitrary -- pluginConfigArgsValue :: [Text]
    
instance Arbitrary PluginConfigInterface where
  arbitrary =
    PluginConfigInterface
      <$> arbitrary -- pluginConfigInterfaceTypes :: [PluginInterfaceType]
      <*> arbitrary -- pluginConfigInterfaceSocket :: Text
    
instance Arbitrary PluginConfigLinux where
  arbitrary =
    PluginConfigLinux
      <$> arbitrary -- pluginConfigLinuxCapabilities :: [Text]
      <*> arbitrary -- pluginConfigLinuxAllowAllDevices :: Bool
      <*> arbitrary -- pluginConfigLinuxDevices :: [PluginDevice]
    
instance Arbitrary PluginConfigNetwork where
  arbitrary =
    PluginConfigNetwork
      <$> arbitrary -- pluginConfigNetworkType :: Text
    
instance Arbitrary PluginConfigRootfs where
  arbitrary =
    PluginConfigRootfs
      <$> arbitrary -- pluginConfigRootfsType :: Maybe Text
      <*> arbitrary -- pluginConfigRootfsDiffIds :: Maybe [Text]
    
instance Arbitrary PluginConfigUser where
  arbitrary =
    PluginConfigUser
      <$> arbitrary -- pluginConfigUserUid :: Maybe Int
      <*> arbitrary -- pluginConfigUserGid :: Maybe Int
    
instance Arbitrary PluginDevice where
  arbitrary =
    PluginDevice
      <$> arbitrary -- pluginDeviceName :: Text
      <*> arbitrary -- pluginDeviceDescription :: Text
      <*> arbitrary -- pluginDeviceSettable :: [Text]
      <*> arbitrary -- pluginDevicePath :: Text
    
instance Arbitrary PluginEnv where
  arbitrary =
    PluginEnv
      <$> arbitrary -- pluginEnvName :: Text
      <*> arbitrary -- pluginEnvDescription :: Text
      <*> arbitrary -- pluginEnvSettable :: [Text]
      <*> arbitrary -- pluginEnvValue :: Text
    
instance Arbitrary PluginInterfaceType where
  arbitrary =
    PluginInterfaceType
      <$> arbitrary -- pluginInterfaceTypePrefix :: Text
      <*> arbitrary -- pluginInterfaceTypeCapability :: Text
      <*> arbitrary -- pluginInterfaceTypeVersion :: Text
    
instance Arbitrary PluginMount where
  arbitrary =
    PluginMount
      <$> arbitrary -- pluginMountName :: Text
      <*> arbitrary -- pluginMountDescription :: Text
      <*> arbitrary -- pluginMountSettable :: [Text]
      <*> arbitrary -- pluginMountSource :: Text
      <*> arbitrary -- pluginMountDestination :: Text
      <*> arbitrary -- pluginMountType :: Text
      <*> arbitrary -- pluginMountOptions :: [Text]
    
instance Arbitrary PluginSettings where
  arbitrary =
    PluginSettings
      <$> arbitrary -- pluginSettingsMounts :: [PluginMount]
      <*> arbitrary -- pluginSettingsEnv :: [Text]
      <*> arbitrary -- pluginSettingsArgs :: [Text]
      <*> arbitrary -- pluginSettingsDevices :: [PluginDevice]
    
instance Arbitrary Port where
  arbitrary =
    Port
      <$> arbitrary -- portIp :: Maybe Text
      <*> arbitrary -- portPrivatePort :: Int
      <*> arbitrary -- portPublicPort :: Maybe Int
      <*> arbitrary -- portType :: Text
    
instance Arbitrary ProcessConfig where
  arbitrary =
    ProcessConfig
      <$> arbitrary -- processConfigPrivileged :: Maybe Bool
      <*> arbitrary -- processConfigUser :: Maybe Text
      <*> arbitrary -- processConfigTty :: Maybe Bool
      <*> arbitrary -- processConfigEntrypoint :: Maybe Text
      <*> arbitrary -- processConfigArguments :: Maybe [Text]
    
instance Arbitrary ProgressDetail where
  arbitrary =
    ProgressDetail
      <$> arbitrary -- progressDetailCode :: Maybe Int
      <*> arbitrary -- progressDetailMessage :: Maybe Int
    
instance Arbitrary PushImageInfo where
  arbitrary =
    PushImageInfo
      <$> arbitrary -- pushImageInfoError :: Maybe Text
      <*> arbitrary -- pushImageInfoStatus :: Maybe Text
      <*> arbitrary -- pushImageInfoProgress :: Maybe Text
      <*> arbitrary -- pushImageInfoProgressDetail :: Maybe ProgressDetail
    
instance Arbitrary Resources where
  arbitrary =
    Resources
      <$> arbitrary -- resourcesCpuShares :: Maybe Int
      <*> arbitrary -- resourcesMemory :: Maybe Int
      <*> arbitrary -- resourcesCgroupParent :: Maybe Text
      <*> arbitrary -- resourcesBlkioWeight :: Maybe Int
      <*> arbitrary -- resourcesBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDevice]
      <*> arbitrary -- resourcesBlkioDeviceReadBps :: Maybe [ThrottleDevice]
      <*> arbitrary -- resourcesBlkioDeviceWriteBps :: Maybe [ThrottleDevice]
      <*> arbitrary -- resourcesBlkioDeviceReadIOps :: Maybe [ThrottleDevice]
      <*> arbitrary -- resourcesBlkioDeviceWriteIOps :: Maybe [ThrottleDevice]
      <*> arbitrary -- resourcesCpuPeriod :: Maybe Integer
      <*> arbitrary -- resourcesCpuQuota :: Maybe Integer
      <*> arbitrary -- resourcesCpuRealtimePeriod :: Maybe Integer
      <*> arbitrary -- resourcesCpuRealtimeRuntime :: Maybe Integer
      <*> arbitrary -- resourcesCpusetCpus :: Maybe Text
      <*> arbitrary -- resourcesCpusetMems :: Maybe Text
      <*> arbitrary -- resourcesDevices :: Maybe [DeviceMapping]
      <*> arbitrary -- resourcesDiskQuota :: Maybe Integer
      <*> arbitrary -- resourcesKernelMemory :: Maybe Integer
      <*> arbitrary -- resourcesMemoryReservation :: Maybe Integer
      <*> arbitrary -- resourcesMemorySwap :: Maybe Integer
      <*> arbitrary -- resourcesMemorySwappiness :: Maybe Integer
      <*> arbitrary -- resourcesNanoCpUs :: Maybe Integer
      <*> arbitrary -- resourcesOomKillDisable :: Maybe Bool
      <*> arbitrary -- resourcesPidsLimit :: Maybe Integer
      <*> arbitrary -- resourcesUlimits :: Maybe [ResourcesUlimits]
      <*> arbitrary -- resourcesCpuCount :: Maybe Integer
      <*> arbitrary -- resourcesCpuPercent :: Maybe Integer
      <*> arbitrary -- resourcesIoMaximumIOps :: Maybe Integer
      <*> arbitrary -- resourcesIoMaximumBandwidth :: Maybe Integer
    
instance Arbitrary ResourcesBlkioWeightDevice where
  arbitrary =
    ResourcesBlkioWeightDevice
      <$> arbitrary -- resourcesBlkioWeightDevicePath :: Maybe Text
      <*> arbitrary -- resourcesBlkioWeightDeviceWeight :: Maybe Int
    
instance Arbitrary ResourcesUlimits where
  arbitrary =
    ResourcesUlimits
      <$> arbitrary -- resourcesUlimitsName :: Maybe Text
      <*> arbitrary -- resourcesUlimitsSoft :: Maybe Int
      <*> arbitrary -- resourcesUlimitsHard :: Maybe Int
    
instance Arbitrary RestartPolicy where
  arbitrary =
    RestartPolicy
      <$> arbitrary -- restartPolicyName :: Maybe Text
      <*> arbitrary -- restartPolicyMaximumRetryCount :: Maybe Int
    
instance Arbitrary Secret where
  arbitrary =
    Secret
      <$> arbitrary -- secretId :: Maybe Text
      <*> arbitrary -- secretVersion :: Maybe NodeVersion
      <*> arbitrary -- secretCreatedAt :: Maybe Text
      <*> arbitrary -- secretUpdatedAt :: Maybe Text
      <*> arbitrary -- secretSpec :: Maybe ServiceSpec
    
instance Arbitrary SecretSpec where
  arbitrary =
    SecretSpec
      <$> arbitrary -- secretSpecName :: Maybe Text
      <*> arbitrary -- secretSpecLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- secretSpecData :: Maybe [Text]
    
instance Arbitrary Service where
  arbitrary =
    Service
      <$> arbitrary -- serviceId :: Maybe Text
      <*> arbitrary -- serviceVersion :: Maybe NodeVersion
      <*> arbitrary -- serviceCreatedAt :: Maybe Text
      <*> arbitrary -- serviceUpdatedAt :: Maybe Text
      <*> arbitrary -- serviceSpec :: Maybe ServiceSpec
      <*> arbitrary -- serviceEndpoint :: Maybe ServiceEndpoint
      <*> arbitrary -- serviceUpdateStatus :: Maybe ServiceUpdateStatus
    
instance Arbitrary ServiceEndpoint where
  arbitrary =
    ServiceEndpoint
      <$> arbitrary -- serviceEndpointSpec :: Maybe EndpointSpec
      <*> arbitrary -- serviceEndpointPorts :: Maybe [EndpointPortConfig]
      <*> arbitrary -- serviceEndpointVirtualIPs :: Maybe [ServiceEndpointVirtualIPs]
    
instance Arbitrary ServiceEndpointVirtualIPs where
  arbitrary =
    ServiceEndpointVirtualIPs
      <$> arbitrary -- serviceEndpointVirtualIPsNetworkId :: Maybe Text
      <*> arbitrary -- serviceEndpointVirtualIPsAddr :: Maybe Text
    
instance Arbitrary ServiceSpec where
  arbitrary =
    ServiceSpec
      <$> arbitrary -- serviceSpecName :: Maybe Text
      <*> arbitrary -- serviceSpecLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- serviceSpecTaskTemplate :: Maybe TaskSpec
      <*> arbitrary -- serviceSpecMode :: Maybe ServiceSpecMode
      <*> arbitrary -- serviceSpecUpdateConfig :: Maybe ServiceSpecUpdateConfig
      <*> arbitrary -- serviceSpecNetworks :: Maybe [TaskSpecNetworks]
      <*> arbitrary -- serviceSpecEndpointSpec :: Maybe EndpointSpec
    
instance Arbitrary ServiceSpecMode where
  arbitrary =
    ServiceSpecMode
      <$> arbitrary -- serviceSpecModeReplicated :: Maybe ServiceSpecModeReplicated
      <*> arbitrary -- serviceSpecModeGlobal :: Maybe A.Value
    
instance Arbitrary ServiceSpecModeReplicated where
  arbitrary =
    ServiceSpecModeReplicated
      <$> arbitrary -- serviceSpecModeReplicatedReplicas :: Maybe Integer
    
instance Arbitrary ServiceSpecUpdateConfig where
  arbitrary =
    ServiceSpecUpdateConfig
      <$> arbitrary -- serviceSpecUpdateConfigParallelism :: Maybe Integer
      <*> arbitrary -- serviceSpecUpdateConfigDelay :: Maybe Integer
      <*> arbitrary -- serviceSpecUpdateConfigFailureAction :: Maybe Text
      <*> arbitrary -- serviceSpecUpdateConfigMonitor :: Maybe Integer
      <*> arbitrary -- serviceSpecUpdateConfigMaxFailureRatio :: Maybe Double
    
instance Arbitrary ServiceUpdateResponse where
  arbitrary =
    ServiceUpdateResponse
      <$> arbitrary -- serviceUpdateResponseWarnings :: Maybe [Text]
    
instance Arbitrary ServiceUpdateStatus where
  arbitrary =
    ServiceUpdateStatus
      <$> arbitrary -- serviceUpdateStatusState :: Maybe Text
      <*> arbitrary -- serviceUpdateStatusStartedAt :: Maybe Text
      <*> arbitrary -- serviceUpdateStatusCompletedAt :: Maybe Text
      <*> arbitrary -- serviceUpdateStatusMessage :: Maybe Text
    
instance Arbitrary SwarmSpec where
  arbitrary =
    SwarmSpec
      <$> arbitrary -- swarmSpecName :: Maybe Text
      <*> arbitrary -- swarmSpecLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- swarmSpecOrchestration :: Maybe SwarmSpecOrchestration
      <*> arbitrary -- swarmSpecRaft :: Maybe SwarmSpecRaft
      <*> arbitrary -- swarmSpecDispatcher :: Maybe SwarmSpecDispatcher
      <*> arbitrary -- swarmSpecCaConfig :: Maybe SwarmSpecCAConfig
      <*> arbitrary -- swarmSpecEncryptionConfig :: Maybe SwarmSpecEncryptionConfig
      <*> arbitrary -- swarmSpecTaskDefaults :: Maybe SwarmSpecTaskDefaults
    
instance Arbitrary SwarmSpecCAConfig where
  arbitrary =
    SwarmSpecCAConfig
      <$> arbitrary -- swarmSpecCAConfigNodeCertExpiry :: Maybe Integer
      <*> arbitrary -- swarmSpecCAConfigExternalCAs :: Maybe [SwarmSpecCAConfigExternalCAs]
    
instance Arbitrary SwarmSpecCAConfigExternalCAs where
  arbitrary =
    SwarmSpecCAConfigExternalCAs
      <$> arbitrary -- swarmSpecCAConfigExternalCAsProtocol :: Maybe Text
      <*> arbitrary -- swarmSpecCAConfigExternalCAsUrl :: Maybe Text
      <*> arbitrary -- swarmSpecCAConfigExternalCAsOptions :: Maybe (Map.Map String Text)
    
instance Arbitrary SwarmSpecDispatcher where
  arbitrary =
    SwarmSpecDispatcher
      <$> arbitrary -- swarmSpecDispatcherHeartbeatPeriod :: Maybe Integer
    
instance Arbitrary SwarmSpecEncryptionConfig where
  arbitrary =
    SwarmSpecEncryptionConfig
      <$> arbitrary -- swarmSpecEncryptionConfigAutoLockManagers :: Maybe Bool
    
instance Arbitrary SwarmSpecOrchestration where
  arbitrary =
    SwarmSpecOrchestration
      <$> arbitrary -- swarmSpecOrchestrationTaskHistoryRetentionLimit :: Maybe Integer
    
instance Arbitrary SwarmSpecRaft where
  arbitrary =
    SwarmSpecRaft
      <$> arbitrary -- swarmSpecRaftSnapshotInterval :: Maybe Integer
      <*> arbitrary -- swarmSpecRaftKeepOldSnapshots :: Maybe Integer
      <*> arbitrary -- swarmSpecRaftLogEntriesForSlowFollowers :: Maybe Integer
      <*> arbitrary -- swarmSpecRaftElectionTick :: Maybe Int
      <*> arbitrary -- swarmSpecRaftHeartbeatTick :: Maybe Int
    
instance Arbitrary SwarmSpecTaskDefaults where
  arbitrary =
    SwarmSpecTaskDefaults
      <$> arbitrary -- swarmSpecTaskDefaultsLogDriver :: Maybe SwarmSpecTaskDefaultsLogDriver
    
instance Arbitrary SwarmSpecTaskDefaultsLogDriver where
  arbitrary =
    SwarmSpecTaskDefaultsLogDriver
      <$> arbitrary -- swarmSpecTaskDefaultsLogDriverName :: Maybe Text
      <*> arbitrary -- swarmSpecTaskDefaultsLogDriverOptions :: Maybe (Map.Map String Text)
    
instance Arbitrary Task where
  arbitrary =
    Task
      <$> arbitrary -- taskId :: Maybe Text
      <*> arbitrary -- taskVersion :: Maybe NodeVersion
      <*> arbitrary -- taskCreatedAt :: Maybe Text
      <*> arbitrary -- taskUpdatedAt :: Maybe Text
      <*> arbitrary -- taskName :: Maybe Text
      <*> arbitrary -- taskLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- taskSpec :: Maybe TaskSpec
      <*> arbitrary -- taskServiceId :: Maybe Text
      <*> arbitrary -- taskSlot :: Maybe Int
      <*> arbitrary -- taskNodeId :: Maybe Text
      <*> arbitrary -- taskStatus :: Maybe TaskStatus
      <*> arbitrary -- taskDesiredState :: Maybe TaskState
    
instance Arbitrary TaskSpec where
  arbitrary =
    TaskSpec
      <$> arbitrary -- taskSpecContainerSpec :: Maybe TaskSpecContainerSpec
      <*> arbitrary -- taskSpecResources :: Maybe TaskSpecResources
      <*> arbitrary -- taskSpecRestartPolicy :: Maybe TaskSpecRestartPolicy
      <*> arbitrary -- taskSpecPlacement :: Maybe TaskSpecPlacement
      <*> arbitrary -- taskSpecForceUpdate :: Maybe Int
      <*> arbitrary -- taskSpecNetworks :: Maybe [TaskSpecNetworks]
      <*> arbitrary -- taskSpecLogDriver :: Maybe TaskSpecLogDriver
    
instance Arbitrary TaskSpecContainerSpec where
  arbitrary =
    TaskSpecContainerSpec
      <$> arbitrary -- taskSpecContainerSpecImage :: Maybe Text
      <*> arbitrary -- taskSpecContainerSpecCommand :: Maybe [Text]
      <*> arbitrary -- taskSpecContainerSpecArgs :: Maybe [Text]
      <*> arbitrary -- taskSpecContainerSpecEnv :: Maybe [Text]
      <*> arbitrary -- taskSpecContainerSpecDir :: Maybe Text
      <*> arbitrary -- taskSpecContainerSpecUser :: Maybe Text
      <*> arbitrary -- taskSpecContainerSpecLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- taskSpecContainerSpecTty :: Maybe Bool
      <*> arbitrary -- taskSpecContainerSpecMounts :: Maybe [Mount]
      <*> arbitrary -- taskSpecContainerSpecStopGracePeriod :: Maybe Integer
      <*> arbitrary -- taskSpecContainerSpecDnsConfig :: Maybe TaskSpecContainerSpecDNSConfig
    
instance Arbitrary TaskSpecContainerSpecDNSConfig where
  arbitrary =
    TaskSpecContainerSpecDNSConfig
      <$> arbitrary -- taskSpecContainerSpecDNSConfigNameservers :: Maybe [Text]
      <*> arbitrary -- taskSpecContainerSpecDNSConfigSearch :: Maybe [Text]
      <*> arbitrary -- taskSpecContainerSpecDNSConfigOptions :: Maybe [Text]
    
instance Arbitrary TaskSpecLogDriver where
  arbitrary =
    TaskSpecLogDriver
      <$> arbitrary -- taskSpecLogDriverName :: Maybe Text
      <*> arbitrary -- taskSpecLogDriverOptions :: Maybe (Map.Map String Text)
    
instance Arbitrary TaskSpecNetworks where
  arbitrary =
    TaskSpecNetworks
      <$> arbitrary -- taskSpecNetworksTarget :: Maybe Text
      <*> arbitrary -- taskSpecNetworksAliases :: Maybe [Text]
    
instance Arbitrary TaskSpecPlacement where
  arbitrary =
    TaskSpecPlacement
      <$> arbitrary -- taskSpecPlacementConstraints :: Maybe [Text]
    
instance Arbitrary TaskSpecResources where
  arbitrary =
    TaskSpecResources
      <$> arbitrary -- taskSpecResourcesLimits :: Maybe TaskSpecResourcesLimits
      <*> arbitrary -- taskSpecResourcesReservation :: Maybe TaskSpecResourcesReservation
    
instance Arbitrary TaskSpecResourcesLimits where
  arbitrary =
    TaskSpecResourcesLimits
      <$> arbitrary -- taskSpecResourcesLimitsNanoCpUs :: Maybe Integer
      <*> arbitrary -- taskSpecResourcesLimitsMemoryBytes :: Maybe Integer
    
instance Arbitrary TaskSpecResourcesReservation where
  arbitrary =
    TaskSpecResourcesReservation
      <$> arbitrary -- taskSpecResourcesReservationNanoCpUs :: Maybe Integer
      <*> arbitrary -- taskSpecResourcesReservationMemoryBytes :: Maybe Integer
    
instance Arbitrary TaskSpecRestartPolicy where
  arbitrary =
    TaskSpecRestartPolicy
      <$> arbitrary -- taskSpecRestartPolicyCondition :: Maybe Text
      <*> arbitrary -- taskSpecRestartPolicyDelay :: Maybe Integer
      <*> arbitrary -- taskSpecRestartPolicyMaxAttempts :: Maybe Integer
      <*> arbitrary -- taskSpecRestartPolicyWindow :: Maybe Integer
    
instance Arbitrary TaskStatus where
  arbitrary =
    TaskStatus
      <$> arbitrary -- taskStatusTimestamp :: Maybe Text
      <*> arbitrary -- taskStatusState :: Maybe TaskState
      <*> arbitrary -- taskStatusMessage :: Maybe Text
      <*> arbitrary -- taskStatusErr :: Maybe Text
      <*> arbitrary -- taskStatusContainerStatus :: Maybe TaskStatusContainerStatus
    
instance Arbitrary TaskStatusContainerStatus where
  arbitrary =
    TaskStatusContainerStatus
      <$> arbitrary -- taskStatusContainerStatusContainerId :: Maybe Text
      <*> arbitrary -- taskStatusContainerStatusPid :: Maybe Int
      <*> arbitrary -- taskStatusContainerStatusExitCode :: Maybe Int
    
instance Arbitrary ThrottleDevice where
  arbitrary =
    ThrottleDevice
      <$> arbitrary -- throttleDevicePath :: Maybe Text
      <*> arbitrary -- throttleDeviceRate :: Maybe Integer
    
instance Arbitrary Volume where
  arbitrary =
    Volume
      <$> arbitrary -- volumeName :: Text
      <*> arbitrary -- volumeDriver :: Text
      <*> arbitrary -- volumeMountpoint :: Text
      <*> arbitrary -- volumeStatus :: Maybe (Map.Map String A.Value)
      <*> arbitrary -- volumeLabels :: (Map.Map String Text)
      <*> arbitrary -- volumeScope :: Text
      <*> arbitrary -- volumeOptions :: (Map.Map String Text)
      <*> arbitrary -- volumeUsageData :: Maybe VolumeUsageData
    
instance Arbitrary VolumeConfig where
  arbitrary =
    VolumeConfig
      <$> arbitrary -- volumeConfigName :: Maybe Text
      <*> arbitrary -- volumeConfigDriver :: Maybe Text
      <*> arbitrary -- volumeConfigDriverOpts :: Maybe (Map.Map String Text)
      <*> arbitrary -- volumeConfigLabels :: Maybe (Map.Map String Text)
    
instance Arbitrary VolumeUsageData where
  arbitrary =
    VolumeUsageData
      <$> arbitrary -- volumeUsageDataSize :: Int
      <*> arbitrary -- volumeUsageDataRefCount :: Int
    
instance Arbitrary HostConfig where
  arbitrary =
    HostConfig
      <$> arbitrary -- hostConfigCpuShares :: Maybe Int
      <*> arbitrary -- hostConfigMemory :: Maybe Int
      <*> arbitrary -- hostConfigCgroupParent :: Maybe Text
      <*> arbitrary -- hostConfigBlkioWeight :: Maybe Int
      <*> arbitrary -- hostConfigBlkioWeightDevice :: Maybe [ResourcesBlkioWeightDevice]
      <*> arbitrary -- hostConfigBlkioDeviceReadBps :: Maybe [ThrottleDevice]
      <*> arbitrary -- hostConfigBlkioDeviceWriteBps :: Maybe [ThrottleDevice]
      <*> arbitrary -- hostConfigBlkioDeviceReadIOps :: Maybe [ThrottleDevice]
      <*> arbitrary -- hostConfigBlkioDeviceWriteIOps :: Maybe [ThrottleDevice]
      <*> arbitrary -- hostConfigCpuPeriod :: Maybe Integer
      <*> arbitrary -- hostConfigCpuQuota :: Maybe Integer
      <*> arbitrary -- hostConfigCpuRealtimePeriod :: Maybe Integer
      <*> arbitrary -- hostConfigCpuRealtimeRuntime :: Maybe Integer
      <*> arbitrary -- hostConfigCpusetCpus :: Maybe Text
      <*> arbitrary -- hostConfigCpusetMems :: Maybe Text
      <*> arbitrary -- hostConfigDevices :: Maybe [DeviceMapping]
      <*> arbitrary -- hostConfigDiskQuota :: Maybe Integer
      <*> arbitrary -- hostConfigKernelMemory :: Maybe Integer
      <*> arbitrary -- hostConfigMemoryReservation :: Maybe Integer
      <*> arbitrary -- hostConfigMemorySwap :: Maybe Integer
      <*> arbitrary -- hostConfigMemorySwappiness :: Maybe Integer
      <*> arbitrary -- hostConfigNanoCpUs :: Maybe Integer
      <*> arbitrary -- hostConfigOomKillDisable :: Maybe Bool
      <*> arbitrary -- hostConfigPidsLimit :: Maybe Integer
      <*> arbitrary -- hostConfigUlimits :: Maybe [ResourcesUlimits]
      <*> arbitrary -- hostConfigCpuCount :: Maybe Integer
      <*> arbitrary -- hostConfigCpuPercent :: Maybe Integer
      <*> arbitrary -- hostConfigIoMaximumIOps :: Maybe Integer
      <*> arbitrary -- hostConfigIoMaximumBandwidth :: Maybe Integer
      <*> arbitrary -- hostConfigBinds :: Maybe [Text]
      <*> arbitrary -- hostConfigContainerIdFile :: Maybe Text
      <*> arbitrary -- hostConfigLogConfig :: Maybe HostConfigLogConfig
      <*> arbitrary -- hostConfigNetworkMode :: Maybe Text
      <*> arbitrary -- hostConfigPortBindings :: Maybe (Map.Map String HostConfigPortBindings)
      <*> arbitrary -- hostConfigRestartPolicy :: Maybe RestartPolicy
      <*> arbitrary -- hostConfigAutoRemove :: Maybe Bool
      <*> arbitrary -- hostConfigVolumeDriver :: Maybe Text
      <*> arbitrary -- hostConfigVolumesFrom :: Maybe [Text]
      <*> arbitrary -- hostConfigMounts :: Maybe [Mount]
      <*> arbitrary -- hostConfigCapAdd :: Maybe [Text]
      <*> arbitrary -- hostConfigCapDrop :: Maybe [Text]
      <*> arbitrary -- hostConfigDns :: Maybe [Text]
      <*> arbitrary -- hostConfigDnsOptions :: Maybe [Text]
      <*> arbitrary -- hostConfigDnsSearch :: Maybe [Text]
      <*> arbitrary -- hostConfigExtraHosts :: Maybe [Text]
      <*> arbitrary -- hostConfigGroupAdd :: Maybe [Text]
      <*> arbitrary -- hostConfigIpcMode :: Maybe Text
      <*> arbitrary -- hostConfigCgroup :: Maybe Text
      <*> arbitrary -- hostConfigLinks :: Maybe [Text]
      <*> arbitrary -- hostConfigOomScoreAdj :: Maybe Int
      <*> arbitrary -- hostConfigPidMode :: Maybe Text
      <*> arbitrary -- hostConfigPrivileged :: Maybe Bool
      <*> arbitrary -- hostConfigPublishAllPorts :: Maybe Bool
      <*> arbitrary -- hostConfigReadonlyRootfs :: Maybe Bool
      <*> arbitrary -- hostConfigSecurityOpt :: Maybe [Text]
      <*> arbitrary -- hostConfigStorageOpt :: Maybe (Map.Map String Text)
      <*> arbitrary -- hostConfigTmpfs :: Maybe (Map.Map String Text)
      <*> arbitrary -- hostConfigUtsMode :: Maybe Text
      <*> arbitrary -- hostConfigUsernsMode :: Maybe Text
      <*> arbitrary -- hostConfigShmSize :: Maybe Int
      <*> arbitrary -- hostConfigSysctls :: Maybe (Map.Map String Text)
      <*> arbitrary -- hostConfigRuntime :: Maybe Text
      <*> arbitrary -- hostConfigConsoleSize :: Maybe [Int]
      <*> arbitrary -- hostConfigIsolation :: Maybe Text
    



instance Arbitrary E'Availability where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Condition where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ContentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FailureAction where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Isolation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Mode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Name where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Protocol where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Role where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Scope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TaskState where
  arbitrary = arbitraryBoundedEnum
