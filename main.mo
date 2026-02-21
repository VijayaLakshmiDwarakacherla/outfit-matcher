import Time "mo:core/Time";
import List "mo:core/List";
import Map "mo:core/Map";
import Principal "mo:core/Principal";

import MixinStorage "blob-storage/Mixin";
import Storage "blob-storage/Storage";

actor {
  include MixinStorage();

  type UserId = Principal;
  type ClothingType = { #top; #bottom; #outerwear; #accessory };
  type Color = { hue : Nat; saturation : Nat; brightness : Nat };
  type PatternType = { #casual; #formal; #sporty };

  type ClothingItem = {
    userId : UserId;
    itemId : Text;
    uploadedAt : Time.Time;
    clothingType : ClothingType;
    color : Color;
    pattern : PatternType;
    blob : Storage.ExternalBlob;
  };

  type MatchScore = Nat;
  type ClothingPair = {
    top : ClothingItem;
    bottom : ClothingItem;
    score : MatchScore;
  };

  let clothingItems = Map.empty<Principal, List.List<ClothingItem>>();

  public shared ({ caller }) func uploadClothingItem(itemId : Text, clothingType : ClothingType, color : Color, pattern : PatternType, blob : Storage.ExternalBlob) : async () {
    let item : ClothingItem = {
      userId = caller;
      itemId;
      uploadedAt = Time.now();
      clothingType;
      color;
      pattern;
      blob;
    };

    let existingItems = switch (clothingItems.get(caller)) {
      case (null) { List.empty<ClothingItem>() };
      case (?items) { items };
    };

    existingItems.add(item);
    clothingItems.add(caller, existingItems);
  };

  public query ({ caller }) func getClothingItems(userId : UserId) : async [ClothingItem] {
    switch (clothingItems.get(userId)) {
      case (null) { [] };
      case (?items) { items.toArray() };
    };
  };

  public query ({ caller }) func generateOutfitPairs(userId : UserId) : async [ClothingPair] {
    let pairs = List.empty<ClothingPair>();
    let items = switch (clothingItems.get(userId)) {
      case (null) { return [] };
      case (?items) { items };
    };

    for (top in items.values()) {
      if (top.clothingType == #top) {
        for (bottom in items.values()) {
          if (bottom.clothingType == #bottom) {
            let pair = {
              top;
              bottom;
              score = calculateMatchScore(top, bottom);
            };
            pairs.add(pair);
          };
        };
      };
    };
    pairs.toArray();
  };

  func calculateMatchScore(top : ClothingItem, bottom : ClothingItem) : MatchScore {
    if (top.pattern != bottom.pattern) { return 50 };

    let colorDifference : Nat = abs((top.color.hue % 180) - (bottom.color.hue % 180));
    if (colorDifference >= 135) { return 30 };
    if (colorDifference <= 45) { return 10 };
    15 * colorDifference;
  };

  func abs(value : Int) : Nat {
    if (value < 0) { (-value).toNat() } else { value.toNat() };
  };
};
