The solution avoids `unsafePerformIO` and manages the list's lifecycle appropriately.  Instead of directly creating the list and storing it, it processes the list element by element without keeping it entirely in memory at once. This approach makes the solution much less likely to cause a space leak. 

```haskell
import System.IO

generateLargeList :: Int -> IO ()
generateLargeList n = do
  forM_ [1..n] $ 
 -> do
    -- Process each element individually. For example, printing it.
    -- This prevents a giant list from being held in memory
    hPutStrLn stdout (show i)

main :: IO ()
main = do
  generateLargeList 10000000
```
By avoiding `unsafePerformIO` and processing the list incrementally, this avoids the memory issue. The solution still handles a large number of elements, but it processes them one by one, ensuring that memory usage remains more controlled.  More complex stream-processing techniques could also be used for even better scalability.