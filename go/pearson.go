package pearson

/* Get an initial hash value by passing in the length of the array. */
func Init(length int) (hash byte) { return byte(length % 256) }

/* Given the hash of a string S0..SN and the character SN+1,
get the hash of the string S0..SN+1.
*/
func FeedByte(oldhash byte, next byte) (newhash byte) {
  /* The following table is the result of the pseudorandom order at
  http://burtleburtle.net/bob/hash/pearson.html implemented as follows:

  x := make([]byte, 256);
  for i := 0; i < 256; i++ {
    x[i] = byte(i)
  }
  k := 7
  for j := 0; j < 4; j++ {
    for i := 0; i < 256; i++ {
      s := x[i]
      k = (k+int(s)) % 256
      x[i] = x[k]
      x[k] = s
    }
  }
  */
  return []byte{
    49, 118, 63, 252, 13, 155, 114, 130, 137, 40, 210, 62, 219, 246, 136, 221,
    174, 106, 37, 227, 166, 25, 139, 19, 204, 212, 64, 176, 70, 11, 170, 58,
    146, 24, 123, 77, 184, 248, 108, 251, 43, 171, 12, 141, 126, 41, 95, 142,
    167, 46, 178, 235, 30, 75, 45, 208, 110, 230, 226, 50, 32, 112, 156, 180,
    205, 68, 202, 203, 82, 7, 247, 217, 223, 71, 116, 76, 6, 31, 194, 183,
    15, 102, 97, 215, 234, 240, 53, 119, 52, 47, 179, 99, 199, 8, 101, 35,
    65, 132, 154, 239, 148, 51, 216, 74, 93, 192, 42, 86, 165, 113, 89, 48,
    100, 195, 29, 211, 169, 38, 57, 214, 127, 117, 59, 39, 209, 88, 1, 134,
    92, 163, 0, 66, 237, 22, 164, 200, 85, 9, 190, 129, 111, 172, 231, 14,
    181, 206, 128, 23, 187, 73, 149, 193, 241, 236, 197, 159, 55, 125, 196, 60,
    161, 238, 245, 94, 87, 157, 122, 158, 115, 207, 17, 20, 145, 232, 107, 16,
    21, 185, 33, 225, 175, 253, 81, 182, 67, 243, 69, 220, 153, 5, 143, 3,
    26, 213, 147, 222, 105, 188, 229, 191, 72, 177, 250, 135, 152, 121, 218, 44,
    120, 140, 138, 28, 84, 186, 198, 131, 54, 2, 56, 78, 173, 151, 83, 27,
    255, 144, 249, 189, 104, 4, 168, 98, 162, 150, 254, 242, 109, 34, 133, 224,
    228, 79, 103, 201, 160, 90, 18, 61, 10, 233, 91, 80, 124, 96, 244, 36,
  }[byte(int(oldhash + next) % 256)]
}

// Get the hash of a []byte.
func Hash(msg []byte) byte {
  hash := Init(len(msg))
  for _, v := range msg { hash = FeedByte(hash, v) }
  return hash
}

// Get the hash a string.
// This just interprets the string as a []byte,
// using Hash().
func HashString(msg string) byte { return Hash([]byte(msg)) }
