---
title: Functional Image Processing in Rust - Part 1
tags: rust, image processing
---

For a so-called 'systems' programming language, Rust is surprisingly
expressive. Enough so, that one can encode image processing routines in a
delightfully functional way. This is the first post in a series of posts where
I walk through implementing concepts of Conal Elliot's `Pan` library for
functional image processing.

<div></div><!--more-->

It's common for developers to have a personal problem to kick the tires of a
new language when learning it. For me, it's typically some variation of Conal
Elliot's `Pan` library for image processing. `Pan` was originally implemented
in Haskell and there's a [Functional Images
chapter](http://conal.net/papers/functional-images/) in the [Fun of Programming
book](https://www.cs.ox.ac.uk/publications/books/fop/) that describes the key
concepts, primitives and abstractions in `Pan`. While it was published some
time back, I still find `Pan`’s simplicity and composability compelling. I
initially implemented some of `Pan`'s concepts in Scala some time ago (perhaps
a blog post for another time), but over the Christmas holidays I thought I'd
see if I could express some of the concepts in Rust and, hopefully, learn some
Rust in the process.

# Functional Img's in Rust

**TL;DR:** I had a few issues finding an appropriate way to express the notion
of an `Img<A>` in Rust initially. Traits along with the recently added `impl
Trait` were what I landed on. Skip [ahead](#link1) if you aren't interested in
the details of how I worked my way towards that.

> What is an image?
> <footer class="blockquote-footer">Conal Elliot, LambdaJam 2017</footer>

What is an image? This seemingly simple question typically yields an array of
answers, usually including some mention of a 2D/3D arrays or matrices. That's
precisely what came to mind when I first thought about the question. It turns
out there's a simpler and more elegant way to represent images: A function that
takes a set of coordinates and produces a pixel value. In Haskell, it looks
something like this:

```haskell
type Coord = (Float, Float)
type Img a = Coord -> a
```

`Img` represents a mapping from that coordinate (Coord) to a pixel value `a`,
where `Coord` is just an alias for a Tuple of floating point numbers that
represent a coordinate in a continuous 2D space. There are a couple of
important concepts embedded in this definition that are worth calling out.
First, coordinates are in a continuous space, which is quite different from
representing images as 2D arrays/matrices with discrete coordinates. Second,the
definition is polymorphic over the pixel type (i.e., it's some `a`), which
means we're not committing to a concrete representation for pixels yet.

Can we express this in Rust? Rust supports anonymous functions via closures,
which have an instance of the `Fn` trait (or typeclass). I naively assumed that
`Fn(Coord) -> A` might be the type of a closure and, since rust also supports
type aliases, I tried to create a type alias for `Fn(Coord) -> A`, similar to
the Haskell implementation:

```rust
type Coord = (f32, f32);
type Img<A> = Fn(Coord) -> A;
```

The code block above doesn't actually compile because one can't alias traits in
Rust; however, there's a more general problem here: `Fn(Coord) -> A` is a trait
(or type bound) rather than a type. Whatever type does represent our `Img<A>`
should satisfy this bound/constraint (i.e., the type should have an instance of
`Fn(Coord) -> A`), but it is not a type itself. I don't want to get too deep
into all my initial misconceptions about closures in Rust, so I'll cut a long
story short: every closure in Rust has a different type and it's difficult to
refer to the type without Boxing the closure--something we'd like to avoid.
Aliasing a closure is, therefore, a bit of a dead-end.

<a name="link1"></a>Rust recently added the ability to return an anonymous
implementation of a trait using `impl Trait`. Thus, we can create a trait that
extends `Fn(Coord) -> A` and use this as a type bound on functions that accept
or produce images. Put another way, for a type to fulfil the `Img<A>` type
bound, it should also fulfil the `Fn(Coord) -> A` constraint.

```rust
type Coord = (f32, f32);
trait Img<A> : Fn(Coord) -> A {}
impl <F, A> Img<A> for F where F: Fn(Coord) -> A {}
```

If you're not familiar with Rust that might look a little intimidating, so
let's break it down. In the first line, we declare a new trait `Img<A>` that
extends `Fn(Coord) -> A`. In the second line, we provide a mechanism to lift
any function that satisfies the constraint `Fn(Coord) -> A` to an `Img<A>`.

# Image regions

Now that we have a representation for `Img`, let's express another type
featured in the Functional Images book chapter: `Region`. `Region`'s, of type
`Img<bool>`, represent a mask where the pixel value, a boolean, denotes whether
that pixel falls inside the region or not. We can express it as follows:

```rust
trait Region : Img<bool> {}
impl <F> Region for F where F: Fn(Coord) -> bool {}
```

# Drawing Img's

Before we move onto seeing what sort of images we can express with out types so
far, I'd like to digress for a moment into how we actually render bitmaps for
our abstract `Img` representation—after all, the beauty of an image lies in
seeing it. Instead of writing all our own logic for encoding different image
formats, we're just going to make use an existing library to do it. The crew at
`PistonDevelopers` have published a nice image processing library called
[`image`](https://github.com/PistonDevelopers/image) that can encode a variety
of image formats, including `png` and `jpeg`. I'll leave the deep dive into
`image` as an exercise for the reader; however, it's worth pointing out a
couple of important structures and functions we'll be making use of.
`ImageBuffer` is one of the internal representations for images in `image`, and
the function `from_fn` let's one create an `ImageBuffer` from a function that
accepts the `x` and `y` coordinates of the image. This maps very nicely to the
`Fn(Coord) -> A` representation for images given above.

We'll focus on rendering binary `Img`'s, i.e. `Img<bool>`, for purposes of this
post; rendering grayscale and colour images uses a similar pattern, which you
can see in . The `image` library denotes grayscale pixels with the `Luma` type
and only officially supports a single underlying datatype, `u8`, so our target
output type needs to be `ImageBuffer<Luma<u8>>`. For the sake of simplicity,
let's restrict the input type to `Img<f32>`, which means our `render` function
needs to scale and convert the pixels to `u8` dynamic range:

```rust
type Vector = (f32, f32);

fn render<F: Region>(
    im: F,
    width: u32,
    height: u32,
    origin: Vector,
    pixel_size: Vector,
) -> ImageBuffer<Luma<u8>, Vec<u8>> {
    let (pw, ph) = pixel_size;
    let (ox, oy) = origin;
    image::ImageBuffer::from_fn(width, height, |x, y| {
        let pixel = im((x as f32 * pw - ox, y as f32 * ph - oy)) as u8;
        image::Luma([pixel * 255])
    })
}
```

## Creating and drawing Regions

Let's define a couple of functions that create `Region`s and see what they look
like. `vstrip` defines an infinite vertical band that is centred on the y-axis
of the image:

```rust
pub fn vstrip() -> impl Region {
    |(x, _y): Coord| x.abs() <= 0.5
}
```

![Vertical strip - `render(vstrip(), 512, 512, (2.5, 0.0), (0.009765625, 0.009765625))`](../../images/strip.png){.img-fluid .rounded .mx-auto .d-block}

One of my favourites, along with various close derivatives, is `checker`:

```rust
pub fn checker() -> impl Region {
    |(x, y): Coord| ((x.floor() + y.floor()) as u32).is_even()
}
```

![Checker - `render(checker(), 1024, 512, (0.0, 0.0), (0.009765625, 0.009765625))`](../../images/checker.png){.full-width}

We can also define a function that will produce alternating rings around the
origin of the image:

```rust
pub fn dist_o(c: Coord) -> f32 {
    let (x, y) = c;
    (x * x + y * y).sqrt()
}

pub fn alt_rings() -> impl Region {
    |p| (dist_o(p).floor() as i32).is_even()
}
```

![Alternating rings - `render(alt_rings(), 512, 512, (10.0, 10.0), (0.0390625, 0.0390625))`](../../images/rings.png){.img-fluid .rounded .mx-auto .d-block}

It’s often easier to define functions using a Polar coordinate system as
opposed to the cartesian coordinate system. In a polar coordinate system, a
given coordinate is defined by its distance (or radius) and angle from the
origin.

```rust
pub type Polar = (f32, f32);

pub fn to_polar(c: Coord) -> Polar {
    let (x, y) = c;
    (dist_o(c), y.atan2(x))
}

pub fn from_polar(polar: Polar) -> Coord {
    let (p, theta) = polar;
    (p * theta.cos(), p * theta.sin())
}

pub fn compose<A, B, C, F, G>(f: F, g: G) -> impl Fn(A) -> C + Copy
where
    F: Fn(B) -> C + Copy,
    G: Fn(A) -> B + Copy,
{
    move |x| f(g(x))
}

pub fn polar_checker(n: i32) -> impl Region {
    let sc = move |polar: Polar| -> Polar {
        let (p, theta) = polar;
        (p, theta * (n as f32 / PI))
    };
    compose(checker(), compose(sc, to_polar))
}
```

![Polar Checker - `render(polar_checker(10), 1024, 512, (5.0, 2.5), (0.009765625, 0.009765625))`](../../images/ring_checker.png){.full-width}


This is probably a good place to end this post as it’s getting quite long. In
follow up posts, we'll take a look at how to encode and render colour images,
perform image transformations (e.g., crop, scale, rotate,  etc.), and filter
images.

