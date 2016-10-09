use quickcheck;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct MyUnityType;

impl quickcheck::Arbitrary for MyUnityType {
    fn arbitrary<G: quickcheck::Gen>(_: &mut G) -> MyUnityType {
        MyUnityType
    }
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct MyNewtype(usize);
impl quickcheck::Arbitrary for MyNewtype {
    fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> MyNewtype {
        MyNewtype(usize::arbitrary(g))
    }
}

impl quickcheck::Arbitrary for StructTuple {
    fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> StructTuple {
        StructTuple(i32::arbitrary(g), i32::arbitrary(g))
    }
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct StructTuple(i32, i32);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Point {
    x: i32,
    y: i32,
}

impl quickcheck::Arbitrary for Point {
    fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> Point {
        Point {
            x: i32::arbitrary(g),
            y: i32::arbitrary(g),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum SomeEnum {
    Foo,
    Quux,
    Bar(u64),
    Baz {
        some: i32,
    },
}

impl quickcheck::Arbitrary for SomeEnum {
    fn arbitrary<G: quickcheck::Gen>(g: &mut G) -> SomeEnum {
        match u64::arbitrary(g) % 4 {
            0 => SomeEnum::Foo,
            1 => SomeEnum::Quux,
            2 => SomeEnum::Bar(quickcheck::Arbitrary::arbitrary(g)),
            3 => SomeEnum::Baz { some: quickcheck::Arbitrary::arbitrary(g) },
            n => panic!("Unexpected value mod 4: {:?}", n),
        }
    }

    fn shrink(&self) -> Box<Iterator<Item = SomeEnum> + 'static> {
        // writeln!(std::io::stderr(),"shrink: {:?}", self).unwrap();
        match *self {
            SomeEnum::Foo => quickcheck::empty_shrinker(),
            SomeEnum::Quux => quickcheck::single_shrinker(SomeEnum::Foo),
            SomeEnum::Bar(ref x) => {
                let chained = quickcheck::single_shrinker(SomeEnum::Foo)
                                  .chain(x.shrink().map(SomeEnum::Bar));
                Box::new(chained)
            }
            SomeEnum::Baz { some: x } => {
                let chained = quickcheck::single_shrinker(SomeEnum::Foo)
                                  .chain(x.shrink().map(|n| SomeEnum::Baz { some: n }));
                Box::new(chained)
            }
        }
    }
}


