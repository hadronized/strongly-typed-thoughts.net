I have been doing [Advent of Code 2021](https://adventofcode.com), like every year since 2018, and I would like to share
some reflections about it. This year, I would like to talk about the implementation I made for [day
16](https://adventofcode.com/2021/day/16).

<!-- vim-markdown-toc GFM -->

* [--- Day 16: Packet Decoder --- // reflections](#----day-16-packet-decoder------reflections)
  * [Header parsing](#header-parsing)
  * [The encoding problem](#the-encoding-problem)
  * [The other encoding](#the-other-encoding)
  * [Bit packing solution](#bit-packing-solution)
  * [Packed next_bits](#packed-next_bits)
  * [Decoding a literal packet](#decoding-a-literal-packet)
  * [Parsing operators](#parsing-operators)
  * [Part 1: summing packet headers’ versions](#part-1-summing-packet-headers-versions)
  * [Part 2: evaluating the expression](#part-2-evaluating-the-expression)

<!-- vim-markdown-toc -->

# --- Day 16: Packet Decoder --- // reflections

This problem doesn’t look too hard at the first glance, but has some hidden complexities than I thought pretty
interesting. Just to sum up if you don’t want to open the link, the first part requires you to parse a hexadecimal
number and interpret its binary representation as a _packet_, like what you would find in a network packet format,
basically. You need to be able to parse correctly a single packet, which can contain nested packets. In this puzzle,
packets represent expressions, that can either be:

- A literal number.
- An operation between packets.

Each packet have a header, comprising a header and its type (literal or operator), and then the actual data of the
packet.

From this context, it seems pretty obvious to assume this is going to be a parsing problem. The first part asks you to
parse the packet and its nested packets, and add all the version numbers of the headers of all packets. The format goes
as this (quoting):

> Every packet begins with a standard header: the first three bits encode the packet `version`, and the next three bits
> encode the packet `type ID`. These two values are numbers; all numbers encoded in any packet are represented as binary
> with the most significant bit first. For example, a version encoded as the binary sequence 100 represents the number
> 4.

Then, you have the description of a packet representing a literal number:

> Packets with `type ID 4` represent a literal value. Literal value packets encode a single binary number. To do this,
> the binary number is padded with leading zeroes until its length is a multiple of four bits, and then it is broken
> into groups of four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed by a 0 bit.
> These groups of five bits immediately follow the packet header. For example, the hexadecimal string `D2FE28` becomes:

```
110100101111111000101000
VVVTTTAAAAABBBBBCCCCC
```

> Below each bit is a label indicating its purpose:
>
> - The three bits labeled `V` (`110`) are the packet `version`, `6`.
> - The three bits labeled `T` (`100`) are the packet `type ID`, `4`, which means the packet is a literal value.
> - The five bits labeled `A` (`10111`) start with a `1` (not the last group, keep reading) and contain the first four bits of the number, `0111`.
> - The five bits labeled `B` (`11110`) start with a `1` (not the last group, keep reading) and contain four more bits of the number, `1110`.
> - The five bits labeled `C` (`00101`) start with a `0` (last group, end of packet) and contain the last four bits of the number, `0101`.
> - The three unlabeled `0` bits at the end are extra due to the hexadecimal representation and should be ignored.
>
> So, this packet represents a literal value with binary representation `011111100101`, which is `2021` in decimal.

I’m just going to focus on this first kind of packet because it’s already interesting.

## Header parsing

From this description, if you have ever done bit parsing, you should already know that there is going to be a problem.
You can parse a literal packet using this kind of pseudo code (ignoring errors for simplicity, it’s AoC!):

```
fn parse_packet(&mut self) -> Packet {
  let version = self.next_bits(3);
  let type_id = self.next_bits(3);

  match type_id {
    4 => todo!("literal parser"),
    _ => todo!("operator parser"),
  }
}
```

The big question is: how do we implement `next_bits`? Getting 3 bits of information? As you might know, the smallest
amount of information a computer can manipulate is a _byte_, which is 8 bits, so… how do we manipulate less than that?

## The encoding problem

I would like to say one thing before going on. The input is a hexadecimal string. The first step of your solution is to
extract the binary representation. For instance, `A` (10 in decimal) is encoded as `1010` and `5A` is encoded as
`01011010`. As you can see (might already know), hexadecimal is easy to parse into bytes: group hexadecimal digits by
two, convert each digit to 4 bits, and glue them together. `5A` is `5 -> 0101` and `A -> 1010`, which gives `01011010`
as seen above.

But as we have seen with `next_bits` above, we will want to manipulate bits, not bytes. So you have several choices and
design decisions to make here:

You can go the « easy » way and store each bit as a byte. This is very suboptimal, because you are going to waste 8
times the amount of memory you actually need to store all this. For instance, `5A` (`01011010`), which is a single byte
on your computer, will require 8 bytes to be stored: `00000000, 00000001, 00000000, 00000001, 00000001, 00000000,
00000001, 00000000`. That is very inefficient, but it would work, if you put them in a `Vec<u8>`. To do that, you would
basically need to read one hexadecimal digit at a time (which has 4 bits), and then perform bitwise right shifts with
masking (`& 0x1`) to extract the bit. Something like:

```rust
fn inefficient_bit_extract(&mut self, four_bits: u8) {
  for i in 0..4 {
    self.bits.push((four_bits >> i) & 0x1);
  }
}
```

With this solution, implementing `next_bits` is easier, because you only need to `pop` the `Vec<u8>`. Let’s write
`next_bits` with this kind of implementation. Because we do not need more than 15 bits in this puzzle (explained later in
the puzzle text, not really important for now), we will return the read bits on `u16`:

```rust
fn next_bits(&mut self, n: usize) -> u16 {
  assert!(n < 16);

  let mut out = 0;

  for i in 0..n {
    out = (out << i) | self.bits.pop() as u16;
  }

  out
}
```

Let’s explain quickly.

```rust
    out = (out << i) | self.bits.pop() as u16;
```

This will extract a single bit, like `00000001`, will convert it to 16-bit (so `0000000000000001`) and will `OR` it to
the number we are building. That number is left shifted at each iteration so that we only `OR` the least significant bit
at each iteration. For instance, if we call `next_bits(3)` and we get `101`, this implementation will basically store
this in `out` at each iteration:

- End of 1st iteration: `0000000000000001`.
- End of 2nd iteration: `0000000000000010`.
- End of 3nd iteration: `0000000000000101`.

Easy, and works.

## The other encoding

However, I am a perfectionist. This is not satisfying to me, and it has been years since I have been wanting to
implement something like what I am about to describe. I have been thinking about this problem with a different context
but it is similar: a `bool` value, in a computer, is stored on the minimal amount of data a computer can manipulate:
a byte. It means than storing a boolean value wastes 7 bits. If you have a single boolean value, there is nothing you
can do. But if you need several boolean values, like, six booleans… using 6 bytes still wastes 42 bits for 6 bits of
useful data! And 6 bits can hold in a single byte. So we should be able to store booleans in a packed array of bytes.
The idea is the following: imagine that you want to store six boolean value in a byte:

```
00100101
ABCDEFGH
```

I have labelled each bit with a letter to make it easier to reference them. If you want to, for instance, the boolean
value stored at index 2 (`F` in our representation), all you have to do is to right shift that number using the index of
the element and 1-bit mask:

```rust
fn read_boolean(byte: u8, index: usize) -> u8 {
  assert!(index < 8);

  (byte >> index) & 0x1
}
```

If you want to set it, you need to do the opposite operation using a logical `OR`:

```rust
fn set_boolean(byte: &mut u8, index: usize) {
  *byte = *byte | (0x1 << index);
}
```

I wanted to use this AoC 18 day to encode my solution as a packed array of bits, and still have the `next_bits`
interface that would allow me to get between 0 and 15 bits of data, encoded as `u16`. The catch is: because I bit pack
the hexadecimal number, it is going to lie in a `Vec<u8>`. And the whole complexity relies in the fact asking for `N`
bits can span across several bytes in the packed array…

## Bit packing solution

The first thing we need to do is to think about the interface. I want that mutable `next_bits` interface. What it means
is that I want to be able to read as many bits as I want (given less than 16 bits), I should not have to worry about the
byte implications behind the scenes. The puzzle text clearly shows that when we are going to parse a literal value, we
will have to read groups of 5 bits. Reading two literal numbers already implies two bytes (10 bits), and even the first
group implies reading two bytes, because before that, you have the header on 6 bits (3 bits for the version, 3 bits for
the type ID), living 2 bits to read for the beginning of the group, and 3 bits in the next byte to read. That’s a lot of
complexity that should be hidden from the interface.

Let’s start with the `Decoder` type:

```rust
#[derive(Clone, Debug)]
struct Decoder {
  bytes: Vec<u8>,
  bitpos: usize, // the position in bit in the byte array
}
```

This is fairly simple: `bytes` is the bit-packed array I mentioned, and `bitpos` is the current position in that array.
But it’s not the position in the _array elements_, it’s the position in _bits_. If `bytes` has 3 bytes (24 bits), then
`bitpos` can be between `0` and `23`. To know in which actual array element the bit is, we simply need to divide by 8.
For instance, if `bitpos = 17`, then `17 / 8 = 2` tells us that the bit is in `bytes[2]`. To know the actual bit
position in that byte, you will have guessed it, you need to take the modulo: `17 % 8 = 1`, which is the second most
significant bit. You can convert to least significant bit by subtracting that number to 8: `8 - 1 = 7`.

With all that information, let’s fill the bytes:

```rust
impl Decoder {
  fn new(input: &str) -> Self {
    let digits: Vec<_> = input.chars().flat_map(|hexa| hexa.to_digit(16)).collect();
    let bytes = digits.chunks(2).map(|d| (d[0] << 4 | d[1]) as u8).collect();

    Self { bytes, bitpos: 0 }
  }
```

Here, because this is AoC, I don’t care about errors, and hence use `flat_map` to remove `Option` or `Result` in
iterators. The important thing here is that I group hexadecimal digits by 2 (the `.chunks(2)` part), and then simply do
this:

```rust
|d| (d[0] << 4 | d[1]) as u8
```

`d[0]` are the most significant 4 bits, so I left shift them by 4 bits, and `d[1]` is already correctly positioned, so
nothing to do with it. I convert to `u8` parse `to_digit(16)` yields `u32`.

Then, let’s implement `next_bits`.

## Packed next_bits

```rust
  fn next_bits(&mut self, n: usize) -> u16 {
    // how many bits are still available from the current byte
    //
    // 0000000011111111
    //      ^ position is 5, so we have 3 bits still available
    let bits_avail = 8 - self.bitpos % 8;

    if bits_avail >= n {
      // we have enough in the current byte; compute the new position and right shift to align correctly
      let shift = bits_avail - n;
      let byte = (self.bytes[self.bitpos / 8] as u16 >> shift) & ((1 << n) - 1);
      self.bitpos += n;
      byte
    } else {
      // …
    }
```

The first step to do is to check whether the amount of bits requested would make us overlap with another byte given the
current `bitpos`. If it’s not the case, then the problem is trivial. As shown in the example, we can just simply right
shift by the number of available bits minus the requested amount (because we might still have room afterwards), and mask
the N least significant bits. By the way, this is a funny one: setting to 1 the N significant bits. If you have `N = 3`,
you want (on 8-bit here to simplify) `00000111`. How do you make that quickly? It’s actually quite simple: what is this
number? This `111`? It’s an odd number (because its least significant bit is `1`) and it looks like it’s almost 2³. It’s
actually 2³ - 1. And this makes sense: remember in school / when you were learning binary that the biggest number you
can represent on N bits is `2^n - 1`. Like, on 8-bit, 2⁸ is 256, and the biggest number you can represent is 255, as you
might already know, which is `11111111`. So, we can simply deduce `00111…1` is the biggest number for 2^N where N is the
number of `1`. And that number is `2^N - 1`. A power of 2, in binary, is a simple shift. So, `2^N` is `1 << N`. And `2^N
- 1` is… `(1 << N) - 1`. You have it.

We then increment `bitpos` by the number of read bits, and return the actual byte, for which the read bits are right
shifted.

Now, what happens when `bits_avail < n`? We have to read `bits_avail` and shift them in the 16-bit output. Then we will
have to switch to the next byte, and read `n - bits_avail` additional bits, because `n` is the requested number of bits
to read and since we read every bits available from the current bytes, well, `n - bits_avail`. The « byte switching »
logic can be a bit overwhelming at first but is actually not that hard: once we have read `bits_avail`, we know that the
next byte will have 8 bits available. We can then divide the problem in two sub-problems, easier:

1. Read and shift 8 bits of data as long as `n >= 8`.
2. Read the remaining bits in the last byte.

This logic is implemented in the `} else {` branch, so I will just copy the `else` and put the code, and explain it:

```rust
    } else {
      // this is the annoying part; first, get all the bits from the current byte
      let byte0 = (self.bytes[self.bitpos / 8] as u16) & ((1 << bits_avail) - 1);
      self.bitpos += bits_avail;

      // then, left shift those bits as most significant bits for the final number
      let mut final_byte = byte0;

      // then, we need to get the remaining bits; compute the number of bytes we need
      let n = n - bits_avail;
      let bytes_needed = n / 8;

      // while we require more than 1 byte, the problem is trivial; just copy and left shift the bytes
      for _ in 1..=bytes_needed {
        let byte = self.bytes[self.bitpos / 8] as u16;
        self.bitpos += 8;
        final_byte = (final_byte << 8) | byte;
      }

      // the remaining bits can be extracted from the last byte
      let n = n % 8;
      let shift = 8 - n;
      let byte = (self.bytes[self.bitpos / 8] as u16 >> shift) & ((1 << n) - 1);
      self.bitpos += n;

      final_byte = (final_byte << n) | byte;

      final_byte
    }
  }
```

Let’s dive in.

```rust
      let byte0 = (self.bytes[self.bitpos / 8] as u16) & ((1 << bits_avail) - 1);
      self.bitpos += bits_avail;
```

This is the current byte, from which, as said earlier, we need to read all available bits. Because we are going to read
everything from the current position to the least significant bit, we can just read the whole byte and apply a mask for
the `bits_avail` least significant bits (remember the explanation earlier). So `& ((1 << bits_avail) - 1)` does the job.
Because we have read `bits_avail` bits, we increment `bitpos` as well.

```rust
      // then, left shift those bits as most significant bits for the final number
      let mut final_byte = byte0;
```

This part is not strictly needed as I could have called `byte0` `final_byte` in the first place, but I wanted a clear
mind about what was doing what. The idea for the following steps is that we are going to accumulate bits in `final_byte`
by left shifting it before applying the logical `OR`. The amount of shift to apply depends on the amount of bits read.
For the case where we read a whole byte, we will just right shift by 8 bits.

```rust
      // then, we need to get the remaining bits; compute the number of bytes we need
      let n = n - bits_avail;
      let bytes_needed = n / 8;
```

This is a pretty straight-forward code given what I said above. `n - bits_avail` is the remaining amount of bits to
read. `bytes_needed` is the number of full bytes we will have to go through (in `self.bytes`), and from which we will
extract 8 bits each. The rest then makes more sense:

```rust
      // while we require more than 1 byte, the problem is trivial; just copy and left shift the bytes
      for _ in 1..=bytes_needed {
        let byte = self.bytes[self.bitpos / 8] as u16;
        self.bitpos += 8;
        final_byte = (final_byte << 8) | byte;
      }
```

Then, the remaining bits are the last part of the parsing problem. The idea is that we know we will have to read between
1 and 7 bits from that byte. That information is stored as `n` and `shift`:

```rust
      let n = n % 8;
      let shift = 8 - n;
```

We then extract the information, and return the N-bit number:

```rust
      let byte = (self.bytes[self.bitpos / 8] as u16 >> shift) & ((1 << n) - 1);
      self.bitpos += n;

      final_byte = (final_byte << n) | byte;

      final_byte
```

## Decoding a literal packet

Armed with `next_bits`, the problem really becomes trivial. Let’s decode a literal packet and introduce the `Packet`
type:

```rust
#[derive(Clone, Debug, Eq, PartialEq)]
struct Header {
  version: u8,
  type_id: u8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Packet {
  Literal {
    header: Header,
    value: u128,
  },

  Operator {
    header: Header,
    length_type_id: u8,
    packets: Vec<Packet>,
  },
}
```

And `Decoder::decode`:

```rust
impl Decoder {
  // …

  fn decode(&mut self) -> Packet {
    let version = self.next_bits(3) as u8;
    let type_id = self.next_bits(3) as u8;
    let header = Header { version, type_id };

    match type_id {
      // literal value
      4 => {
        let mut value = 0u128;
        loop {
          let bits = self.next_bits(5) as u128;
          value = (value << 4) | (bits & 0xF);

          if (bits >> 4) & 0x1 == 0 {
            break Packet::Literal { header, value };
          }
        }
      }

      // operator
      _ => {
        todo!()
      }
    }
  }
}
```

Some explanation. Given the format, we know that a literal number is made of packets of 5 bits, where the most
significant bit does not belong to the number, but instead is a flag. If set to `0`, it means that the literal is over.

```rust
          value = (value << 4) | (bits & 0xF);
```

This accumulates the 4 least significant bits of the 5 bits into the actual literal number.

```rust
          if (bits >> 4) & 0x1 == 0 {
```

And this performs the test on the most significant bit to check whether the literal number is fully built.

## Parsing operators

Operators represent the other kind of packets, and are introduced, quoting:

> Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some
> calculation on one or more sub-packets contained within. Right now, the specific operations aren't important;
> focus on parsing the hierarchy of sub-packets.
>
> An operator packet contains one or more packets. To indicate which subsequent binary data represents its
> sub-packets, an operator packet can use one of two modes indicated by the bit immediately after the packet
> header; this is called the length type ID:
>
> - If the length `type ID` is `0`, then the next `15` bits are a number that represents the total length in bits of the
>   sub-packets contained by this packet.
> - If the length `type ID` is `1`, then the next `11` bits are a number that represents the number of sub-packets
>   immediately contained by this packet.
>
> Finally, after the `length type ID` bit and the 15-bit or 11-bit field, the sub-packets appear.

There is nothing else to do than just applying those rules:

```rust
      // in Decoder::decode’s match on type ID

      // operator
      _ => {
        let length_type_id = self.next_bits(1) as u8;
        let mut packets = Vec::new();

        if length_type_id == 0 {
          let total_length_bits = self.next_bits(15) as usize;
          let mut bits_read = 0;

          while bits_read < total_length_bits {
            let bitpos = self.bitpos;
            packets.push(self.decode());
            bits_read += self.bitpos - bitpos;
          }
        } else {
          let sub_packets_nb = self.next_bits(11);

          for _ in 0..sub_packets_nb {
            packets.push(self.decode());
          }
        }

        Packet::Operator {
          header,
          length_type_id,
          packets,
        }
      }
```

Because of the `next_bits` interface, for when `length_type_id == 0`, we have no way to know deterministically when to
stop, because we don’t really know how long each packet is. Because of this, the `bits_read` variable is introduced so
that we know how many bits we read. That inforation is easy to get by comparing the value of `bitpos` before and after
reading a packet. For the other branch, there is nothing interesting to comment here.

## Part 1: summing packet headers’ versions

The first question was:

> Decode the structure of your hexadecimal-encoded BITS transmission; what do you get if you add up the version numbers
> in all packets?

And my input was (it was a single string but I break it on several lines, because it’s pretty long):

```
60552F100693298A9EF0039D24B129BA56D67282E600A4B5857002439CE580E5E5AEF67803600D2E294B2FCE8AC489BAEF37FEACB31A678548034EA0
086253B183F4F6BDDE864B13CBCFBC4C10066508E3F4B4B9965300470026E92DC2960691F7F3AB32CBE834C01A9B7A933E9D241003A520DF31664700
2E57C1331DFCE16A249802DA009CAD2117993CD2A253B33C8BA00277180390F60E45D30062354598AA4008641A8710FCC01492FB75004850EE5210AC
EF68DE2A327B12500327D848028ED0046661A209986896041802DA0098002131621842300043E3C4168B12BCB6835C00B6033F480C493003C4008002
9F1400B70039808AC30024C009500208064C601674804E870025003AA400BED8024900066272D7A7F56A8FB0044B272B7C0E6F2392E3460094FAA500
2512957B98717004A4779DAECC7E9188AB008B93B7B86CB5E47B2B48D7CAD3328FB76B40465243C8018F49CA561C979C182723D76964220041275627
1FC80460A00CC0401D8211A2270803D10A1645B947B3004A4BA55801494BC330A5BB6E28CCE60BE6012CB2A4A854A13CD34880572523898C7EDE1A9F
A7EED53F1F38CD418080461B00440010A845152360803F0FA38C7798413005E4FB102D004E6492649CC017F004A448A44826AB9BFAB5E0AA8053306B
0CE4D324BB2149ADDA2904028600021909E0AC7F0004221FC36826200FC3C8EB10940109DED1960CCE9A1008C731CB4FD0B8BD004872BC8C3A432BC8
C3A4240231CF1C78028200F41485F100001098EB1F234900505224328612AF33A97367EA00CC4585F315073004E4C2B003530004363847889E200C45
985F140C010A005565FD3F06C249F9E3BC8280804B234CA3C962E1F1C64ADED77D10C3002669A0C0109FB47D9EC58BC01391873141197DCBCEA401E2
CE80D0052331E95F373798F4AF9B998802D3B64C9AB6617080
```

So:

1. Parse the input and decode it into a `Packet`.
2. Recurse on the `version` field of all sub-packets, summing them.

```rust
fn solve1(input: &str) -> u32 {
  let mut decoder = Decoder::new(input);
  checksum(&decoder.decode())
}

fn checksum(packet: &Packet) -> u32 {
  match packet {
    Packet::Literal { header, .. } => header.version as _,
    Packet::Operator {
      header, packets, ..
    } => header.version as u32 + packets.iter().map(checksum).sum::<u32>(),
  }
}
```

Yes, it’s that simple once you have made the right framework. :) Note that this function is not tail-recursive. A
tail-recursive version would probably perform faster, but it was so convenient to write and took me 15s.

## Part 2: evaluating the expression

Part 2 explains that the operator packets are actually expression operators, operating on literal values. Basically,
it’s a whole [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree). The rules are:

- Packets with `type ID` `0` are sum packets - their value is the sum of the values of their sub-packets. If they only
  have a single sub-packet, their value is the value of the sub-packet.
- Packets with `type ID` `1` are product packets - their value is the result of multiplying together the values of their
  sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
- Packets with `type ID` `2` are minimum packets - their value is the minimum of the values of their sub-packets.
- Packets with `type ID` `3` are maximum packets - their value is the maximum of the values of their sub-packets.
- Packets with `type ID` `5` are greater than packets - their value is 1 if the value of the first sub-packet is greater
  than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two
  sub-packets.
- Packets with `type ID` `6` are less than packets - their value is 1 if the value of the first sub-packet is less than
  the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
- Packets with `type ID` `7` are equal to packets - their value is 1 if the value of the first sub-packet is equal to the
  value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.

This is my solution:

```rust
fn solve2(input: &str) -> u64 {
  let mut decoder = Decoder::new(input);
  eval(&decoder.decode())
}

fn eval(packet: &Packet) -> u64 {
  match packet {
    Packet::Literal { value, .. } => *value as _,
    Packet::Operator {
      header, packets, ..
    } => match header.type_id {
      0 => packets.iter().map(eval).sum::<u64>(),
      1 => packets.iter().map(eval).product::<u64>(),
      2 => packets.iter().map(eval).min().unwrap(),
      3 => packets.iter().map(eval).max().unwrap(),
      5 => {
        if eval(&packets[0]) > eval(&packets[1]) {
          1
        } else {
          0
        }
      }
      6 => {
        if eval(&packets[0]) < eval(&packets[1]) {
          1
        } else {
          0
        }
      }
      7 => {
        if eval(&packets[0]) == eval(&packets[1]) {
          1
        } else {
          0
        }
      }

      id => panic!("unknown type id: {}", id),
    },
  }
}
```

And here you have it all. The
[complete solution is available on my GitHub repository for AoC 21](https://github.com/phaazon/advent-of-code-2021/blob/master/day16/src/main.rs).
I don’t think it was the hardest problem, nor the more interesting, but the decision (and challenge) I decided to go
with (bit-packing with a bit-driven interface) made the whole thing much more fun and interesting to me.

I hoped you liked it and that you learned something, especially regarding bitwise operations. That was pretty heavy! I
think that you should also now have an idea about how you could write a specialized version of `Vec<bool>` that
bit-packs booleans value. Obviously, we do not want `Vec<bool>` to actually do that kind of specialization behind the
scenes (think FFI: when getting a `*const bool` or `*mut bool` from the `Vec<bool>`, if those pointers point to
bit-packed data… it’s going to be a nightmare if the FFI function you use expect an actual array of unpacked boolean
values).

For the rest of AoC, I’m probably going to continue working on it but I’m currently pausing at day 18 (Christmas,
fatigue, a bit of OSS burnout too). I will probably write some more about AoC once I feel rested.

Have fun and keep the vibes!
