
use std::fmt::Debug;
use std::collections::Bound;
use std::collections::range::{RangeArgument as RangeArg};
use std::collections::btree_set::{Range as BRange};
use std::collections::BTreeSet;
use std::cmp::{Ordering};
use std::marker::PhantomData;

pub mod leapfrog {
    use super::*;
//    use super::LUB;

    pub trait MonotonicSeekable {
        type Item: Ord + PartialOrd + Eq + PartialEq + Clone;
        fn least_upper_bound(&mut self,&Self::Item) -> Option<Self::Item>;
    }
    pub trait Peekable {
        type Item;
        fn peek(&self) -> Option<Self::Item>;
    }
    pub trait DoublePeekable {
        type Item;
        fn second(&self) -> Option<Self::Item>;
    }

    pub struct UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone,
              T:Iterator<Item=It> + MonotonicSeekable<Item=It> + Peekable<Item=It>
    {
        iterators: Vec<T>,
        count:usize,
        exhausted: bool,
        _it:PhantomData<It>
    }




    impl<T,It> UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone + Debug,
              T:Iterator<Item=It> + MonotonicSeekable<Item=It> + Peekable<Item=It> + Ord + PartialOrd + Eq + PartialEq
    {
        pub fn new(lfis:Vec<T>) -> UnaryJoiner<T,It> {
            let mut lf = lfis;
            lf.sort();
            let count = lf.len();
            assert!(count > 1);
            let mut ret = UnaryJoiner{iterators:lf,count:count,exhausted:false,_it:PhantomData::<It>};
            ret.search();
            ret
        }

        #[inline]
        fn largest_index(&self,p:usize) -> usize {
            (p+self.count-1) % self.count
        }

        fn search(&mut self) {
            // p points to the smallest (or the first of an equally small set)
            let mut p = 0;
            loop {
                match (self.iterators[self.largest_index(p)].peek(),self.iterators[p].peek()) {
                    (Some(ref big),Some(ref small)) => {
                        if big == small {
                            return;
                        } else {
                            () // Continue Searching;
                        }
                    },
                    (None,_) => {self.exhausted = true;return;},
                    (_,None) => {self.exhausted = true;return;}
                }

                // What follows is a work-around to deal with the rust borrow checker.
                // The array of LFIs needs to be split in two so that part of it can be mutated, based on an immutable borrow from the second part.
                if p == 0 {
                    let (mut small_part,big_part) = self.iterators.split_at_mut(1);
                    let ind = big_part.len()-1;
                    small_part[0].least_upper_bound(&big_part[ind].peek().unwrap());
                } else {
                    let (big_part,mut small_part) = self.iterators.split_at_mut(p);
                    let ind = big_part.len()-1;
                    small_part[0].least_upper_bound(&big_part[ind].peek().unwrap());
                }
                p = (p + 1) % (self.count);
            }
        }
    }


    impl<T,It> Iterator for UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone + Debug,
              T:Iterator<Item=It> + MonotonicSeekable<Item=It> + Peekable<Item=It> + Ord + PartialOrd + Eq + PartialEq
    {
        type Item=It;
        fn next(&mut self) -> Option<<Self as Iterator>::Item> {
            if self.exhausted == true {return None}
            let ind = self.largest_index(0);
            let ret = self.iterators[self.count-1].peek();
            self.iterators[ind].next();
            self.search();
            ret
        }
    }
    impl<T,It> Peekable for UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone + Debug,
              T:Iterator<Item=It> + MonotonicSeekable<Item=It> + Peekable<Item=It> + Ord + PartialOrd + Eq + PartialEq
    {
        type Item=It;
        fn peek(&self) -> Option<<Self as Peekable>::Item> {
            self.iterators[self.count-1].peek()
        }
    }
    impl<T,It> MonotonicSeekable for UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone + Debug,
              T:Iterator<Item=It> + MonotonicSeekable<Item=It> + Peekable<Item=It> + Ord + PartialOrd + Eq + PartialEq
    {
        type Item=It;
        fn least_upper_bound(&mut self,seek:&<Self as MonotonicSeekable>::Item) -> Option<<Self as Peekable>::Item> {
            self.iterators[self.count-1].least_upper_bound(&seek);
            self.search();
            self.iterators[self.count-1].peek()
        }
    }

    pub use self::btree_set::*;
    mod btree_set {
        use super::*;
        #[derive(Clone,Debug)]
        pub struct BTreeLeapAdapter<'a,It: 'a> {
            tree:&'a BTreeSet<It>,
            iter:BRange<'a,It>,
            curr:Option<&'a It>
        }

        impl<'a,It: 'a> Iterator for BTreeLeapAdapter<'a,It> {
            type Item=&'a It;
            fn next(&mut self) -> Option<Self::Item> {
                let c = self.iter.next();
                let ret = self.curr;
                self.curr = c;
                ret
            }
        }
        impl<'a,It: 'a> Peekable for BTreeLeapAdapter<'a,It> {
            type Item=&'a It;
            fn peek(&self) -> Option<Self::Item> {
                self.curr
            }
        }
        impl<'a,It: 'a> MonotonicSeekable for BTreeLeapAdapter<'a,It>
            where It:Ord + Clone{

            type Item=&'a It;
            fn least_upper_bound(&mut self,seek:&Self::Item) -> Option<Self::Item> {
                let lower : It = (**seek).clone();
                let mut t = self.tree.range(LUB::<It>(Some(lower)));
                self.curr = t.next();
                self.iter = t;
                self.curr
            }
        }

        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> BTreeLeapAdapter<'a,Val> {
            pub fn new(tree:&'a BTreeSet<Val>) -> BTreeLeapAdapter<'a,Val> {
                let mut iter = tree.range(LUB(None));
                let curr = iter.next();
                BTreeLeapAdapter{tree:tree,iter:iter,curr:curr}
            }
        }

        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> Ord for BTreeLeapAdapter<'a,Val> {
            fn cmp(&self,other:&Self) -> Ordering {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.cmp(&rhs)},
                    (None,Some(_)) => Ordering::Greater,
                    (Some(_),None) => Ordering::Less,
                    (None,None) => Ordering::Equal
                }
            }
        }
        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> Eq for BTreeLeapAdapter<'a,Val> {}
        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> PartialEq for BTreeLeapAdapter<'a,Val> {
            fn eq(&self,other:&Self) -> bool {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.eq(&rhs)},
                    (None,None) => true,
                    _ => false
                }
            }
        }
        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> PartialOrd for BTreeLeapAdapter<'a,Val> {
            fn partial_cmp(&self,other:&Self) -> Option<Ordering> {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.partial_cmp(&rhs)},
                    (None,Some(_)) => Some(Ordering::Greater),
                    (Some(_),None) => Some(Ordering::Less),
                    (None,None) => Some(Ordering::Equal)
                }
            }
        }


    }

    pub mod vec {
        use std::fmt::Debug;
        pub struct VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug
        {
            vec:&'a [T],
            index:usize
        }
        impl<'a,T> VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug{
            pub fn new(vec:&'a [T]) -> VecAdapter<'a,T> {
                VecAdapter{vec:vec,index:0}
            }

            pub fn seek(&mut self,seek:&T) {
                let mut new_ind = match &self.vec[self.index..].binary_search(seek) {
                    &Ok(val) => val,
                    &Err(val) => val
                };
                if self.vec.len() > (new_ind+self.index) && self.vec[new_ind+self.index] < *seek {
                    new_ind += 1;
                }
                
                self.index += new_ind;
            }
        }

        impl<'a,T> Iterator for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug {
            type Item=&'a T;
            fn next(&mut self) -> Option<Self::Item> {
                if self.index >= self.vec.len() {
                    None
                } else {
                    let ret = &self.vec[self.index];
                    self.index += 1;
                    Some(ret)
                }
            }
        }

        use super::{Peekable,MonotonicSeekable};
        impl<'a,T> Peekable for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug{
            type Item=&'a T;
            fn peek(&self) -> Option<Self::Item> {
                if self.index >= self.vec.len() {
                    None
                } else {
                    Some(&self.vec[self.index])
                }
            }
        }

        impl<'a,T> MonotonicSeekable for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug{
            type Item=&'a T;
            fn least_upper_bound(&mut self, seek:&Self::Item) -> Option<Self::Item> {
                self.seek(seek);
                self.peek()
            }
        }
        use std::cmp::Ordering;

        impl<'a, T> Ord for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug{
            fn cmp(&self,other:&Self) -> Ordering {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.cmp(&rhs)},
                    (None,Some(_)) => Ordering::Greater,
                    (Some(_),None) => Ordering::Less,
                    (None,None) => Ordering::Equal
                }
            }
        }
        impl<'a, T> Eq for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug {}
        impl<'a, T> PartialEq for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug{
            fn eq(&self,other:&Self) -> bool {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.eq(&rhs)},
                    (None,None) => true,
                    _ => false
                }
            }
        }
        impl<'a, T> PartialOrd for VecAdapter<'a,T>
            where T: 'a + Ord + PartialOrd + Eq + PartialEq + Clone + Debug{
            fn partial_cmp(&self,other:&Self) -> Option<Ordering> {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.partial_cmp(&rhs)},
                    (None,Some(_)) => Some(Ordering::Greater),
                    (Some(_),None) => Some(Ordering::Less),
                    (None,None) => Some(Ordering::Equal)
                }
            }
        }

    }


    #[cfg(test)]
    mod tests {
        use quickcheck::*;
        use std::collections::BTreeSet;
        use super::*;
        use super::vec::VecAdapter;

        #[test]
        fn btree_leapfrog_v_set_triangle() {
            fn prop(a:Vec<u8>,b:Vec<u8>,c:Vec<u8>) -> bool {
                let mut bta = BTreeSet::new();
                bta.extend(a.clone().into_iter());
                let mut btb = BTreeSet::new();
                btb.extend(b.clone().into_iter());
                let mut btc = BTreeSet::new();
                btc.extend(c.clone().into_iter());
                let iab = bta.intersection(&btb).cloned()
                    .collect::<BTreeSet<u8>>();
                let iabc= iab.intersection(&btc)
                    .collect::<Vec<&u8>>();
                let lfjs = vec![BTreeLeapAdapter::new(&bta),BTreeLeapAdapter::new(&btb),BTreeLeapAdapter::new(&btc)];
                let leap_frog = UnaryJoiner::new(lfjs);
                let test = leap_frog.collect::<Vec<&u8>>();
                test == iabc
            }
            quickcheck(prop as fn(Vec<u8>,Vec<u8>,Vec<u8>)-> bool);
        }


        #[test]
        fn btree_leapfrog_many() {
            fn prop(data:Vec<Vec<u8>>) -> bool {
                if data.len() < 2 {return true}
                let start = data[0].iter().cloned().collect::<BTreeSet<u8>>();
                let good = data.iter()
                    .fold(start,|acc,new| {
                        let t = new.iter().cloned().collect::<BTreeSet<u8>>();
                        acc.intersection(&t).cloned().collect::<BTreeSet<u8>>()
                    });

                let btrees = data.iter().map(|x| {
                    let mut d = BTreeSet::new();
                    d.extend(x);
                    d
                }).collect::<Vec<BTreeSet<u8>>>();

                let lfjs = btrees.iter().map(|t| {BTreeLeapAdapter::new(t)}).collect();

                let leap_frog = UnaryJoiner::new(lfjs);
                let test = leap_frog.collect::<BTreeSet<&u8>>();

                test == good.iter().collect::<BTreeSet<&u8>>()
            }
            QuickCheck::new()
                .tests(1000)
                .max_tests(5000)
                .quickcheck(prop as fn(Vec<Vec<u8>>)-> bool);
        }


        #[test]
        fn vec_leapfrog_v_set_triangle() {
            fn prop(a:Vec<u8>,b:Vec<u8>,c:Vec<u8>) -> bool {
                let mut va = a.clone();
                va.sort();
                va.dedup();
                let mut vb = b.clone();
                vb.sort();
                vb.dedup();
                let mut vc = c.clone();
                vc.sort();
                vc.dedup();

                let lfjs = vec![VecAdapter::new(&va),VecAdapter::new(&vb),VecAdapter::new(&vc)];
                let leap_frog = UnaryJoiner::new(lfjs);
                let test = leap_frog.collect::<Vec<&u8>>();

                let mut bta = BTreeSet::new();
                bta.extend(a.clone().into_iter());
                let mut btb = BTreeSet::new();
                btb.extend(b.clone().into_iter());
                let mut btc = BTreeSet::new();
                btc.extend(c.clone().into_iter());
                let iab = bta.intersection(&btb).cloned()
                    .collect::<BTreeSet<u8>>();
                let iabc= iab.intersection(&btc)
                    .collect::<Vec<&u8>>();
                test == iabc
            }
            // assert!(prop(vec![2, 7],
            //              vec![1, 6],
            //              vec![0, 1, 5]));
            quickcheck(prop as fn(Vec<u8>,Vec<u8>,Vec<u8>)-> bool);
        }


        #[test]
        fn vec_leapfrog_many() {
            fn prop(data:Vec<Vec<u8>>) -> bool {
                if data.len() < 2 {return true}
                let start = data[0].iter().cloned().collect::<BTreeSet<u8>>();
                let good = data.iter()
                    .fold(start,|acc,new| {
                        let t = new.iter().cloned().collect::<BTreeSet<u8>>();
                        acc.intersection(&t).cloned().collect::<BTreeSet<u8>>()
                    });

                let mut db = data;
                let lfjs = db.iter_mut().map(|t| {
                    t.sort();
                    t.dedup();
                    VecAdapter::new(t)}).collect();

                let leap_frog = UnaryJoiner::new(lfjs);
                let test = leap_frog.collect::<BTreeSet<&u8>>();

                test == good.iter().collect::<BTreeSet<&u8>>()
            }

            QuickCheck::new()
                .tests(1000)
                .max_tests(5000)
                .quickcheck(prop as fn(Vec<Vec<u8>>)-> bool);
        }

    }
}

mod quadzilla {
    use super::*;

    pub trait MonotonicSeekableBuffered {
        type Item: Ord + PartialOrd + Eq + PartialEq + Clone;
        fn least_upper_bound(&mut self,&Self::Item);
    }
    pub trait Peekable {
        type Item;
        fn peek(&self) -> Option<Self::Item>;
    }
    pub trait DoublePeekable {
        type Item;
        fn second(&self) -> Option<Self::Item>;
    }

    pub struct UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone,
              T:Iterator<Item=It> + MonotonicSeekableBuffered<Item=It> + Peekable<Item=It> //+ DoublePeekable<Item=It>
    {
        iterators: Vec<T>,
        exhausted: bool,
        _it:PhantomData<It>
    }

    enum Step<V> {
        NewFloor(V),
        NewCeil(V),
        Match,
        Exhausted
    }

    impl<T,It> UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone + Debug,
              T:Iterator<Item=It> + MonotonicSeekableBuffered<Item=It> + Peekable<Item=It> + DoublePeekable<Item=It>
    {
        pub fn new(lfis:Vec<T>) -> UnaryJoiner<T,It> {
            assert!(lfis.len() > 1);
            let mut ret = UnaryJoiner{iterators:lfis,exhausted:false,_it:PhantomData::<It>};
            ret.search();
            ret
        }

        fn get_step(&self) -> Step<It> {

            let ceil = self.iterators.iter().max_by_key(|x| x.peek()).unwrap().peek();

            let fl = self.iterators.iter()
                .filter( |x| { x.peek() != ceil })
                .max_by(
                    |x,y| {
                        match (x.second(),y.second()) {
                            (None,_) => Ordering::Greater,
                            (_,None) => Ordering::Less,
                            (Some(l),Some(r)) => l.cmp(&r)
                        }
                    });
            match fl {
                None => Step::Match,
                Some(fla) => {
                    let floor = fla.second();
                    if floor == None {
                        Step::Exhausted
                    } else if ceil >= floor {
                        Step::NewFloor(floor.unwrap())
                    } else {
                        Step::NewCeil(floor.unwrap())
                    }
                }
            }
        }

        fn search(&mut self) {
            // This function assumes that we are not currently matching.
            loop {
                match self.get_step() {
                    Step::Exhausted => { self.exhausted = true; return},
                    Step::Match => {return},
                    Step::NewFloor(v) | Step::NewCeil(v) => {
                        for it in self.iterators.iter_mut() {
                            it.least_upper_bound(&v);
                        }
                    },
                }
            }
        }
    }

    impl<T,It> Iterator for UnaryJoiner<T,It>
        where It: Ord + PartialOrd + Eq + PartialEq + Clone + Debug,
              T:Iterator<Item=It> + MonotonicSeekableBuffered<Item=It> + DoublePeekable<Item=It> + Peekable<Item=It> + Ord + PartialOrd + Eq + PartialEq
    {
        type Item=It;
        fn next(&mut self) -> Option<<Self as Iterator>::Item> {
            if self.exhausted == true {return None}
            let ret = self.iterators[0].peek().clone();
            self.iterators.iter_mut().max_by_key(|x| x.second()).and_then(|x| x.next());
            self.search();
            ret
        }
    }

    pub use self::btree_set::*;
    mod btree_set {
        use super::*;
        #[derive(Clone,Debug)]
        pub struct BTreeLeapAdapter<'a,It: 'a> {
            tree:&'a BTreeSet<It>,
            iter:BRange<'a,It>,
            curr:Option<&'a It>,
            second:Option<&'a It>
        }


        impl<'a,It: 'a> Iterator for BTreeLeapAdapter<'a,It> {
            type Item=&'a It;
            fn next(&mut self) -> Option<Self::Item> {
                let c = self.iter.next();
                let ret = self.curr;
                self.curr = self.second;
                self.second = c;
                ret
            }
        }

        impl<'a,It: 'a> Peekable for BTreeLeapAdapter<'a,It> {
            type Item=&'a It;
            fn peek(&self) -> Option<Self::Item> {
                self.curr
            }
        }

        impl<'a,It: 'a> DoublePeekable for BTreeLeapAdapter<'a,It> {
            type Item=&'a It;
            fn second(&self) -> Option<Self::Item> {
                self.second
            }
        }

        impl<'a,It: 'a> MonotonicSeekableBuffered for BTreeLeapAdapter<'a,It>
            where It:Ord + Clone + Debug{

            type Item=&'a It;
            fn least_upper_bound(&mut self,seek:&Self::Item) {
                if self.curr == None {
                    return
                }
                if self.curr.unwrap() >= seek {
                    return
                }
                if self.second == None || self.second.unwrap() >= seek {
                    self.next();
                    return
                }
                let lower : It = (**seek).clone();
                let mut t = self.tree.range(LUB::<It>(Some(lower)));
                self.curr = t.next();
                self.second = t.next();
                self.iter = t;
            }
        }

        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> BTreeLeapAdapter<'a,Val> {
            pub fn new(tree:&'a BTreeSet<Val>) -> BTreeLeapAdapter<'a,Val> {
                let mut iter = tree.range(LUB(None));
                let curr = iter.next();
                let second = iter.next();
                BTreeLeapAdapter{tree:tree,iter:iter,curr:curr,second:second}
            }
        }


        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> Ord for BTreeLeapAdapter<'a,Val> {
            fn cmp(&self,other:&Self) -> Ordering {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.cmp(&rhs)},
                    (None,Some(_)) => Ordering::Greater,
                    (Some(_),None) => Ordering::Less,
                    (None,None) => Ordering::Equal
                }
            }
        }
        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> Eq for BTreeLeapAdapter<'a,Val> {}
        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> PartialEq for BTreeLeapAdapter<'a,Val> {
            fn eq(&self,other:&Self) -> bool {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.eq(&rhs)},
                    (None,None) => true,
                    _ => false
                }
            }
        }
        impl<'a, Val: 'a + Ord + PartialOrd + Eq + PartialEq + Clone> PartialOrd for BTreeLeapAdapter<'a,Val> {
            fn partial_cmp(&self,other:&Self) -> Option<Ordering> {
                match (self.peek(),other.peek()) {
                    (Some(lhs),Some(rhs)) => { lhs.partial_cmp(&rhs)},
                    (None,Some(_)) => Some(Ordering::Greater),
                    (Some(_),None) => Some(Ordering::Less),
                    (None,None) => Some(Ordering::Equal)
                }
            }
        }
    }
    
    #[cfg(test)]
    mod tests {
        use quickcheck::*;
        use std::collections::BTreeSet;
        use super::*;
        

        #[test]
        fn leapfrog_v_set_triangle() {
            fn prop(a:Vec<u8>,b:Vec<u8>,c:Vec<u8>) -> bool {

                let mut bta = BTreeSet::new();
                bta.extend(a.clone().into_iter());
                let mut btb = BTreeSet::new();
                btb.extend(b.clone().into_iter());
                let mut btc = BTreeSet::new();
                btc.extend(c.clone().into_iter());

                let iab = bta.intersection(&btb).cloned()
                    .collect::<BTreeSet<u8>>();
                let iabc= iab.intersection(&btc)
                    .collect::<Vec<&u8>>();

                let lfjs = vec![BTreeLeapAdapter::new(&bta),BTreeLeapAdapter::new(&btb),BTreeLeapAdapter::new(&btc)];

                let leap_frog = UnaryJoiner::new(lfjs);
                let test = leap_frog.collect::<Vec<&u8>>();

                test == iabc
            }
            assert!(prop(vec![72],vec![72],vec![72,0]))
            // quickcheck(prop as fn(Vec<u8>,Vec<u8>,Vec<u8>)-> bool);
        }


        #[test]
        fn leapfrog_many() {
            fn prop(data:Vec<Vec<u8>>) -> bool {
                if data.len() < 2 {return true}
                let start = data[0].iter().cloned().collect::<BTreeSet<u8>>();
                let good = data.iter()
                    .fold(start,|acc,new| {
                        let t = new.iter().cloned().collect::<BTreeSet<u8>>();
                        acc.intersection(&t).cloned().collect::<BTreeSet<u8>>()
                    });

                let btrees = data.iter().map(|x| {
                    let mut d = BTreeSet::new();
                    d.extend(x);
                    d
                }).collect::<Vec<BTreeSet<u8>>>();

                let lfjs = btrees.iter().map(|t| {BTreeLeapAdapter::new(t)}).collect();

                let leap_frog = UnaryJoiner::new(lfjs);
                let test = leap_frog.collect::<BTreeSet<&u8>>();

                test == good.iter().collect::<BTreeSet<&u8>>()
            }

            QuickCheck::new()
                .tests(1000)
                .max_tests(5000)
                .quickcheck(prop as fn(Vec<Vec<u8>>)-> bool);
        }
    }

}

struct LUB<T>(Option<T>);
impl<T> RangeArg<T> for LUB<T> {
    fn start(&self) -> Bound<&T> {
        if let &Some(ref x) = &self.0 {
            Bound::Included(&x)
        } else {
            Bound::Unbounded
        }
    }
    fn end(&self) -> Bound<&T> {Bound::Unbounded}
}


#[cfg(test)]
mod benches {

    use test::Bencher;
    use std::iter::FromIterator;
    use std::iter::{repeat};
    use itertools::*;

    enum D {
        Sparse,
        Dense
    }

    impl D {
        fn get<C:FromIterator<u32>>(&self,seed:u32,size:u32) -> Vec<C> {
            match self {
                &D::Sparse => {
                    let primes : Vec<u32> = (1..seed).collect();
                    let skips : Vec<C> = primes.iter().map(|x| { (0..size).step_by(*x as usize).collect()}).collect();
                    skips
                },
                &D::Dense => {
                    let primes = repeat(2).interleave(repeat(3)).take(seed as usize);
                    let skips : Vec<C> = primes.map(|x| { (0..size).step_by(x as usize).collect()}).collect();
                    skips
                }
            }
        }
    }

    macro_rules! bench_suite {
        () => {
            #[ignore]
            #[bench]
            fn dense_10_100_(b:&mut Bencher) {
                bench_press(D::Dense,10,100,b);
            }
            #[ignore]
            #[bench]
            fn dense_100_100_(b:&mut Bencher) {
                bench_press(D::Dense,100,100,b);
            }
            #[ignore]
            #[bench]
            fn dense_1000_100_(b:&mut Bencher) {
                bench_press(D::Dense,1000,100,b);
            }
            #[ignore]
            #[bench]
            fn dense_10_10000_(b:&mut Bencher) {
                bench_press(D::Dense,10,10000,b);
            }
            #[ignore]
            #[bench]
            fn dense_100_10000_(b:&mut Bencher) {
                bench_press(D::Dense,100,10000,b);
            }
            #[ignore]
            #[bench]
            fn dense_1000_10000_(b:&mut Bencher) {
                bench_press(D::Dense,1000,10000,b);
            }
            #[ignore]
            #[bench]
            fn dense_10_1000000_(b:&mut Bencher) {
                bench_press(D::Dense,10,1000000,b);
            }
            #[ignore]
            #[bench]
            fn dense_100_1000000_(b:&mut Bencher) {
                bench_press(D::Dense,100,1000000,b);
            }
            #[ignore]
            #[bench]
            fn sparse_10_100_(b:&mut Bencher) {
                bench_press(D::Sparse,10,100,b);
            }
            #[ignore]
            #[bench]
            fn sparse_100_100_(b:&mut Bencher) {
                bench_press(D::Sparse,100,100,b);
            }
            #[ignore]
            #[bench]
            fn sparse_1000_100_(b:&mut Bencher) {
                bench_press(D::Sparse,1000,100,b);
            }
            #[ignore]
            #[bench]
            fn sparse_10_10000_(b:&mut Bencher) {
                bench_press(D::Sparse,10,10000,b);
            }
            #[ignore]
            #[bench]
            fn sparse_100_10000_(b:&mut Bencher) {
                bench_press(D::Sparse,100,10000,b);
            }
            #[ignore]
            #[bench]
            fn sparse_1000_10000_(b:&mut Bencher) {
                bench_press(D::Sparse,1000,10000,b);
            }
            #[ignore]
            #[bench]
            fn sparse_10_1000000_(b:&mut Bencher) {
                bench_press(D::Sparse,10,1000000,b);
            }
            #[ignore]
            #[bench]
            fn sparse_100_1000000_(b:&mut Bencher) {
                bench_press(D::Sparse,100,1000000,b);
            }
        }
    }

    mod leapfrog_btree {
        use super::*;
        use ::leapfrog::traits::leapfrog::*;
        use std::collections::BTreeSet;

        fn bench_press(d:D,width:u32,height:u32,b:&mut Bencher) {
            let db = d.get(width,height);
            b.iter(|| {
                let lfis = db.iter().map(|t| {BTreeLeapAdapter::new(t)}).collect();
                let leap_frog = UnaryJoiner::new(lfis);
                leap_frog.collect::<BTreeSet<&u32>>()
            });
        }

        bench_suite!();
    }

    mod leapfrog_vec {
        use super::*;
        use ::leapfrog::traits::leapfrog::*;
        use ::leapfrog::traits::leapfrog::vec::VecAdapter;


        fn bench_press(d:D,width:u32,height:u32,b:&mut Bencher) {
            let db = d.get::<Vec<u32>>(width,height);
            b.iter(|| {
                let lfis = db.iter().map(|t| {VecAdapter::new(t)}).collect();
                let leap_frog = UnaryJoiner::new(lfis);
                leap_frog.collect::<Vec<&u32>>()
            });
        }

        bench_suite!();
    }

    mod quadzilla {
        use super::*;
        use ::leapfrog::traits::quadzilla::*;
        use std::collections::BTreeSet;

        fn bench_press(d:D,width:u32,height:u32,b:&mut Bencher) {
            let db = d.get(width,height);
            b.iter(|| {
                let lfis = db.iter().map(|t| {BTreeLeapAdapter::new(t)}).collect();
                let leap_frog = UnaryJoiner::new(lfis);
                leap_frog.collect::<BTreeSet<&u32>>()
            });
        }

        bench_suite!();
    }

    mod hash_set {
        use super::*;
        use std::collections::HashSet;
        fn bench_press(d:D,width:u32,height:u32,b:&mut Bencher) {
            let db = d.get::<HashSet<u32>>(width,height);
            b.iter(|| {
                let mut it = db.iter();
                let first = it.next().unwrap().clone();
                it.fold(first,|acc,x| {acc.intersection(x).cloned().collect::<HashSet<u32>>()})
            });
        }

        bench_suite!();
    }

    mod btree_set {
        use super::*;
        use std::collections::BTreeSet;

        fn bench_press(d:D,width:u32,height:u32,b:&mut Bencher) {
            let db = d.get::<BTreeSet<u32>>(width,height);

            b.iter(|| {
                let mut it = db.iter();
                let first = it.next().unwrap().clone();
                it.fold(first,|acc,x| {acc.intersection(x).cloned().collect::<BTreeSet<u32>>()})
            });
        }

        bench_suite!();

    }

    mod btree_set_split {
        use super::*;
        use std::collections::BTreeSet;

        fn bench_press(d:D,width:u32,height:u32,b:&mut Bencher) {
            let seed = d.get::<BTreeSet<u32>>(width,height);

            b.iter(move || {
                let mut db = seed.clone();
                while db.len() > 1 {
                    let res = db.iter_mut().chunks(2).into_iter().map(|mut x| {
                        let first = x.next().unwrap().clone();
                        x.fold(first,|acc,n| {acc.intersection(&n).cloned().collect::<BTreeSet<u32>>()})
                    }).collect::<Vec<BTreeSet<u32>>>();
                    db = res;
                }
                db.clone()
            });
        }

        bench_suite!();
    }
    
}
