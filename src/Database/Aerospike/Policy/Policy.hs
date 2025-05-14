module Database.Aerospike.Policy.Policy where

import Data.Int (Int32)
import Data.Word (Word32, Word64)

data BasePolicy = BasePolicy
    { socketTimeout :: Maybe Word32
    -- ^  Socket idle timeout in milliseconds when processing a database command.
    -- If socket_timeout is zero and total_timeout is non-zero, then socket_timeout will be set
    -- to total_timeout.  If both socket_timeout and total_timeout are non-zero and
    -- socket_timeout > total_timeout, then socket_timeout will be set to total_timeout. If both
    -- socket_timeout and total_timeout are zero, then there will be no socket idle limit.
    --
    -- If socket_timeout is non-zero and the socket has been idle for at least socket_timeout,
    -- both max_retries and total_timeout are checked.  If max_retries and total_timeout are not
    -- exceeded, the command is retried.
    --
    -- Default: 30000ms
    , totalTimeout :: Maybe Word32
    -- ^ Total command timeout in milliseconds.
    -- The total_timeout is tracked on the client and sent to the server along with
    -- the command in the wire protocol.  The client will most likely timeout
    -- first, but the server also has the capability to timeout the command.
    --
    -- If total_timeout is not zero and total_timeout is reached before the command
    -- completes, the command will return error AEROSPIKE_ERR_TIMEOUT.
    -- If totalTimeout is zero, there will be no total time limit.
    --
    -- Default: 1000
    , maxRetries :: Maybe Word32
    -- ^ Maximum number of retries before aborting the current command.
    -- The initial attempt is not counted as a retry.
    --
    -- If max_retries is exceeded, the command will return error AEROSPIKE_ERR_TIMEOUT.
    --
    -- WARNING: Database writes that are not idempotent (such as "add")
    -- should not be retried because the write operation may be performed
    -- multiple times if the client timed out previous command attempts.
    -- It's important to use a distinct write policy for non-idempotent
    -- writes which sets max_retries = 0;
    --
    -- Default for read: 2 (initial attempt + 2 retries = 3 attempts)
    --
    -- Default for write: 0 (no retries)
    --
    -- Default for partition scan or query with null filter: 5
    --
    -- No default for legacy scan/query. No retries are allowed for these commands.
    , sleepBetweenRetires :: Maybe Word32
    -- ^ Milliseconds to sleep between retries.  Enter zero to skip sleep.
    -- This field is ignored when max_retries is zero.
    -- This field is also ignored in async mode.
    --
    -- Reads do not have to sleep when a node goes down because the cluster
    -- does not shut out reads during cluster reformation.  The default for
    -- reads is zero.
    --
    -- The default for writes is also zero because writes are not retried by default.
    -- Writes need to wait for the cluster to reform when a node goes down.
    -- Immediate write retries on node failure have been shown to consistently
    -- result in errors.  If max_retries is greater than zero on a write, then
    -- sleep_between_retries should be set high enough to allow the cluster to
    -- reform (>= 3000ms).
    --
    -- Default: 0 (do not sleep between retries).
    }
    deriving stock (Show)

data KeyPolicy
    = -- |
      -- Send the digest value of the key.
      -- This is the recommended mode of operation. This calculates the digest
      -- and send the digest to the server. The digest is only calculated on
      -- the client, and not on the server.
      KeyDigest
    | -- |
      -- Send the key, in addition to the digest value.
      -- If you want keys to be returned when scanning or querying, the keys must
      -- be stored on the server. This policy causes a write operation to store
      -- the key. Once a key is stored, the server will keep it - there is no
      -- need to use this policy on subsequent updates of the record.
      --
      -- If this policy is used on read or delete operations, or on subsequent
      -- updates of a record with a stored key, the key sent will be compared
      -- with the key stored on the server. A mismatch will cause
      -- AEROSPIKE_ERR_RECORD_KEY_MISMATCH to be returned.
      KeySend
    deriving stock (Show)

data ExistsPolicy
    = -- | Write the record, regardless of existence. (i.e. create or update.)
      ExistsIgnore
    | -- | Create a record, ONLY if it doesn't exist.
      ExistsCreate
    | -- | Update a record, ONLY if it exists.
      ExistsUpdate
    | -- | Completely replace a record, ONLY if it exists
      ExistsReplace
    | -- | Completely replace a record if it exists, otherwise create it
      ExistsCreateOrReplace
    deriving stock (Show)

data MapPolicy = MapPolicy
    { attributes :: Word64
    , flags :: Word64
    , itemCommand :: Int32
    , itemsCommand :: Int32
    }
    deriving stock (Show)

data ReadPolicy = ReadPolicy
    { base :: BasePolicy
    , key :: KeyPolicy
    }
    deriving stock (Show)

data WritePolicy = WritePolicy
    { base :: BasePolicy
    , key :: KeyPolicy
    , exists :: ExistsPolicy
    }
    deriving stock (Show)

data BatchPolicy = BatchPolicy
    { base :: BasePolicy
    , concurrent :: Bool
    , allowInline :: Bool
    , allowInlineSsd :: Bool
    , respondAllKeys :: Bool
    , deserialize :: Bool
    }
    deriving stock (Show)

data OperatePolicy = OperatePolicy
    { base :: BasePolicy
    , key :: KeyPolicy
    , exists :: ExistsPolicy
    , deserialize :: Bool
    , respondAllOps :: Bool
    }
    deriving stock (Show)
