{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

class Collects_CC e c where
  empty_CC  :: c e
  insert_CC :: e -> c e -> c e
  member_CC :: e -> c e -> Bool

instance Eq e => Collects_CC e [] where
  empty_CC  = []
  insert_CC = (:)
  member_CC = elem
