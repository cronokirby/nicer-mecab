use mecab::Tagger;

/// A morpheme represents a single part of a sentence with meaning.
///
/// Sentences are split into individual morphemes, which form the sentence together.
/// Each morpheme contains the raw part split off from the sentence as well as
/// information about the morpheme.
#[derive(Clone, Debug, PartialEq)]
pub struct Morpheme {
    /// The raw text associated with this morpheme.
    raw: String,
}

/// This represents the information parsed by the analyzer.
#[derive(Clone, Debug, PartialEq)]
pub struct Sentence {
    /// The list of morphemes composing the sentence.
    morphemes: Vec<Morpheme>,
}

pub fn parse_sentence(sentence: &str) -> Sentence {
    let mut tagger = Tagger::new("");
    tagger.parse_nbest_init(sentence);
    let mecab_out = tagger.next().unwrap();
    let mut morphemes = Vec::new();
    for l in mecab_out.lines() {
        if l == "EOS" {
            break;
        }
        // We know that this index exists, based on mecab output
        let tab_index = l.find('\t').unwrap();
        let (raw, _rest) = l.split_at(tab_index);
        morphemes.push(Morpheme {
            raw: raw.to_string(),
        });
    }
    Sentence { morphemes }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_sentence_works_for_single_words() {
        let sentence = "猫";
        let raw = String::from("猫");
        let result = Sentence {
            morphemes: vec![Morpheme { raw }],
        };
        assert_eq!(result, parse_sentence(sentence));
    }
}
