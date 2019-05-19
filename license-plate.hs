import Test.Hspec
import Data.Char

type Edge = String
type Letter = Char
type Middle = (Int, Int, Int)

type Prefix = Edge
type Suffix = Edge

type LicensePlate = (Prefix, Middle, Suffix)

type Order = Int

nextLicensePlate :: LicensePlate -> Order -> LicensePlate
nextLicensePlate plate 0                            = plate
nextLicensePlate (prefix, (9,9,9), "ZZ")   order    = nextLicensePlate  (nextEdge prefix, (0,0,1), "AA")    (order - 1)
nextLicensePlate (prefix, (9,9,9), suffix) order    = nextLicensePlate  (prefix, (0,0,1), nextEdge suffix)  (order - 1)
nextLicensePlate (prefix,  middle, suffix) order    = nextLicensePlate  (prefix, nextMiddle middle, suffix) (order - 1)

nextMiddle:: Middle -> Middle
nextMiddle (x, 9, 9) = (x + 1,  0,   0)
nextMiddle (x, y, 9) = (x,    y+1,   0)
nextMiddle (x, y, z) = (x,      y, z+1)

nextEdge:: Edge -> Edge
nextEdge "ZZ"       = "AA"
nextEdge [c1, 'Z']  = [nextLetter c1, 'A']
nextEdge [c1,  c2]  = [c1, nextLetter c2]

nextLetter:: Letter -> Letter
nextLetter letter = chr (ord letter + 1)

stringAsPlate:: String -> LicensePlate
stringAsPlate [p1, p2, '-', x, y, z, '-', s1, s2] = ([p1,p2], (toInt x, toInt y, toInt z), [s1,s2])

toInt:: Char -> Int
toInt char = read [char] :: Int

plateAsString:: LicensePlate -> String
plateAsString (prefix, (x,y,z), suffix) = prefix ++ "-" ++ show x ++ show y ++ show z ++ "-" ++ suffix

main = hspec $ do

    describe "Next middle number" $ do
        it "Next middle with only right number affected" $ do
            nextMiddle (0, 0, 1) `shouldBe` (0,0,2)
            nextMiddle (0, 0, 2) `shouldBe` (0,0,3)
            nextMiddle (0, 5, 8) `shouldBe` (0,5,9)

        it "Next middle with right number at 9" $ do
            nextMiddle (0,0,9) `shouldBe` (0,1,0)
            nextMiddle (0,5,9) `shouldBe` (0,6,0)

        it "Next middle having number at 9" $ do
            nextMiddle (0,9,9) `shouldBe` (1,0,0)
            nextMiddle (2,9,9) `shouldBe` (3,0,0)

    describe "Next letter" $ do
        it "Next letter should yield next letter" $ do
            nextLetter 'A' `shouldBe` 'B'
            nextLetter 'G' `shouldBe` 'H'

    describe "Next Edge" $ do
        it "Right letter increment" $ do
            nextEdge "AD" `shouldBe` "AE"

        it "When Right letter reach end, increment right letter and restart at A" $ do
            nextEdge "EZ" `shouldBe` "FA"

        it "When reach ZZ restart at AA" $ do
            nextEdge "ZZ" `shouldBe` "AA"

    describe "Get next License plate" $ do
        it "Incrementing non  999 middle should not affect edges" $ do
            nextLicensePlate ("AA", (8,7,6), "AA") 1 `shouldBe` ("AA", (8,7,7), "AA")
            nextLicensePlate ("ZZ", (1,2,3), "YZ") 1 `shouldBe` ("ZZ", (1,2,4), "YZ")

        it "Incrementing 999 middle with non ZZ suffix should not affect prefix" $ do
            nextLicensePlate ("ED", (9,9,9), "DF") 1 `shouldBe` ("ED", (0,0,1), "DG")

        it "Incrementing all" $ do
            nextLicensePlate ("BF", (9,9,9), "ZZ") 1 `shouldBe` ("BG", (0,0,1), "AA")

        it "Incrementing with different order" $ do
            nextLicensePlate ("BF", (9,9,9), "ZY") 2 `shouldBe` ("BF", (0,0,2), "ZZ")

        it "+100" $ do
            nextLicensePlate ("AZ", (5,6,6), "QS") 100 `shouldBe` ("AZ", (6,6,6), "QS")

    describe "license plate as string" $ do
        it "license plate should be concatenate as string" $ do
            plateAsString ("BF", (9,9,9), "ZZ") `shouldBe` "BF-999-ZZ"


    describe "String as licence plate" $ do
        it "license plate string conversion" $ do
            stringAsPlate "BF-123-ED" `shouldBe` ("BF", (1,2,3), "ED")
