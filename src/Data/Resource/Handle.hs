module Data.Resouce.Handle where

import qualified System.IO as IO
import System.IO (FilePath(..), IOMode(..), BufferMode(..), HandlePosn, SeekMode(..))

type Handle s = Resource s IO.Handle

stdin :: Handle s
stdin = mkResource IO.stdin

stdout :: Handle s
stdout = mkResource IO.stdout

stderr :: Handle s
stderr = mkResource IO.stderr

openFile :: (MonadSafe m, MonadIO m) => FilePath -> IOMode -> RegionT s m (Handle s)
openFile path mode = allocate (liftIO $ IO.openFile path mode) (liftIO . IO.hClose)

hFileSize :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Integer
hFileSize h = withResource h (liftIO . IO.hFileSize)

hSetFileSize :: (MonadSafe m, MonadIO m) => Handle s -> Integer -> RegionT s m ()
hSetFileSize h size = withResource h (\h' -> liftIO $ IO.hSetFileSize h' size)

hIsEOF :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Bool
hIsEOF h = withResource h (liftIO . IO.hIsEOF)

hSetBuffering :: (MonadSafe m, MonadIO m) => Handle s -> BufferMode -> RegionT s m ()
hSetBuffering h buffering = withResource h (\h' -> liftIO $ IO.hSetBuffering h' buffering)

hGetBuffering :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m BufferMode
hGetBuffering h = withResource h (liftIO . IO.hGetBuffering)

hFlush :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m ()
hFlush h = withResource h (liftIO . IO.hFlush)

hGetPosn :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m HandlePosn
hGetPosn h = withResource h (liftIO . IO.hGetPosn)

hSetPosn :: (MonadSafe m, MonadIO m) => Handle s -> HandlePosn -> RegionT s m ()
hSetPosn h posn = withResource h (\h' -> liftIO $ IO.hGetPosn h' posn)

hSeek :: (MonadSafe m, MonadIO m) => Handle s -> SeekMode -> Integer -> RegionT s m ()
hSeek h mode i = withResource h (\h' -> liftIO $ IO.hSeek h' mode i)

hTell :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Integer
hTell h = withResource h (liftIO . IO.hTell)

hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Bool
hIsOpen h = withResource h (liftIO . IO.hIsOpen)
hIsClosed h = withResource h (liftIO . IO.hIsClosed)
hIsReadable h = withResource h (liftIO . IO.hIsReadable)
hIsWritable h = withResource h (liftIO . IO.hIsWritable)
hIsSeekable h = withResource h (liftIO . IO.hIsSeekable)

-- TODO: Use CPP to become portable
hIsTerminalDevice :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Bool
hIsTerminalDevice h = withResource h (liftIO . IO.hIsTerminalDevice)

hSetEcho :: (MonadSafe m, MonadIO m) => Handle s -> Bool -> RegionT s m ()
hSetEcho h echo = withResource h (\h' -> liftIO $ IO.hSetEcho h' echo)

hGetEcho :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Bool
hGetEcho h = withResource h (liftIO . IO.hGetEcho)

hShow :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m String
hShow h = withResource h (liftIO . IO.hShow)

hWaitForInput :: (MonadSafe m, MonadIO m) => Handle s -> Int -> RegionT s m Bool
hWaitForInput h t = withResource h (\h' -> liftIO $ IO.hWaitForInput h' t)

hReady :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Bool
hReady h = withResource h (liftIO . IO.hReady)

hGetChar :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Char
hGetChar h = withResource h (liftIO . IO.hGetChar)

hGetLine :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m String
hGetLine h = withResource h (liftIO . IO.hGetLine)

hLookAhead :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m Char
hLookAhead h = withResource h (liftIO . IO.hLookAhead)

-- TODO: Should I have this? It is a really unsafe function.
hGetContents :: (MonadSafe m, MonadIO m) => Handle s -> RegionT s m String
hGetContents h = withResource h (liftIO . IO.hGetContents)

hPutChar :: (MonadSafe m, MonadIO m) => Handle s -> Char -> RegionT s m ()
hPutChar h c = withResource h (\h' -> liftIO $ IO.hPutChar h' c)

hPutStr :: (MonadSafe m, MonadIO m) => Handle s -> String -> RegionT s m ()
hPutStr h str = withResource h (\h' -> liftIO $ IO.hPutStr h' str)

hPutStrLn :: (MonadSafe m, MonadIO m) => Handle s -> String -> RegionT s m ()
hPutStrLn h str = withResource h (\h' -> liftIO $ IO.hPutStrLn h' str)

hPrint :: (MonadSafe m, MonadIO m, Show a) => Handle s -> a -> RegionT s m ()
hPrint h a = withResource h (\h' -> liftIO $ IO.hPrint h' a)

openBinaryFile :: (MonadSafe m, MonadIO m) => FilePath -> IOMode -> RegionT s m (Handle s)
openBinaryFile path mode = allocate (liftIO $ IO.openBinaryFile path mode) (liftIO . hClose)

hSetBinaryMode :: (MonadSafe m, MonadIO m) => Handle s -> Bool -> RegionT s m ()
hSetBinaryMode h mode = withResource h (\h' -> liftIO $ IO.hSetBinaryMode h' mode)

hPutBuf :: (MonadSafe m, MonadIO m) => Handle s -> Ptr a -> Int -> RegionT s m ()
hPutBuf h ptr len = withResource h (\h' -> liftIO $ IO.hPutBuf h' ptr len)

hGetBuf :: (MonadSafe m, MonadIO m) => Handle s -> Ptr a -> Int -> RegionT s m Int
hGetBuf h ptr len = withResource h (\h' -> liftIO $ IO.hGetBuf h' ptr len)

hGetBufSome :: (MonadSafe m, MonadIO m) => Handle s -> Ptr a -> Int -> RegionT s m Int
hGetBufSome h ptr len = withResource h (\h' -> liftIO $ IO.hGetBufSome h' ptr len)

hPutBufNonBlocking :: (MonadSafe m, MonadIO m) => Handle s -> Ptr a -> Int -> RegionT s m Int
hPutBufNonBlocking h ptr len = withResource h (\h' -> liftIO $ IO.hPutBufNonBlocking h' ptr len)

hGetBufNonBlocking :: (MonadSafe m, MonadIO m) => Handle s -> Ptr a -> Int -> RegionT s m Int
hGetBufNonBlocking h ptr len = withResource h (\h' -> liftIO $ IO.hGetBufNonBlocking h' ptr len)

openTempFile :: (MonadSafe m, MonadIO m) => FilePath -> IOMode -> RegionT s m (FilePath, Handle s)
openTempFile path mode = allocate (liftIO $ IO.openTempFile path mode) (liftIO . hClose)
