use mecab::Tagger;
use std::convert::TryFrom;
use std::fmt;

/// Represents an error generated while parsing a sentence's structure.
///
/// This error exists because there are still some rare parts of Mecab's output
/// not handled by this program. This error can help identify those parts in order
/// to improve this program.
#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    /// There were less than 9 parts to mecab's output for a sentence
    InsufficientParts,
    /// The part of speech couldn't be recognized
    UnknownPartOfSpeech(String),
}

/// This represents a generic part of speech, such as a Noun or a Verb.
///
/// The names for these may not be correct from a grammarian's perspective.
///
/// The Debug trait for this enum uses the raw names, whereas the Display implementation
/// uses the Japanese names from mecab.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PartOfSpeech {
    /// 名詞, A noun, such as "猫".
    Noun,
    /// 動詞, A verb, such as "食べる".
    ///
    /// This is used specifically to mark the verb stem, and not the conjugation.
    Verb,
    /// 助詞, A particle, such as "は".
    ///
    /// This is also used to mark parts of a verb's conjugation, such as the
    /// "て" in "食べて".
    Particle,
}

impl fmt::Display for PartOfSpeech {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            PartOfSpeech::Noun => "名詞",
            PartOfSpeech::Verb => "動詞",
            PartOfSpeech::Particle => "助詞",
        };
        write!(f, "{}", jp)
    }
}

impl TryFrom<&str> for PartOfSpeech {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "名詞" => Ok(PartOfSpeech::Noun),
            "動詞" => Ok(PartOfSpeech::Verb),
            "助詞" => Ok(PartOfSpeech::Particle),
            _ => Err(ParseError::UnknownPartOfSpeech(value.into())),
        }
    }
}

/// A morpheme represents a single part of a sentence with meaning.
///
/// Sentences are split into individual morphemes, which form the sentence together.
/// Each morpheme contains the raw part split off from the sentence as well as
/// information about the morpheme.
#[derive(Clone, Debug, PartialEq)]
pub struct Morpheme {
    /// The raw text associated with this morpheme.
    pub raw: String,
    /// Represents the part of speech corresponding to this morpheme.
    pub part_of_speech: PartOfSpeech,
}

/// This represents the information parsed by the analyzer.
#[derive(Clone, Debug, PartialEq)]
pub struct Sentence {
    /// The list of morphemes composing the sentence.
    pub morphemes: Vec<Morpheme>,
}

pub fn parse_sentence(sentence: &str) -> Result<Sentence, ParseError> {
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
        let (raw, rest) = l.split_at(tab_index);
        // Remove the first tab character
        let rest = &rest[1..];
        let mut parts = rest.split(',');
        let part_of_speech_raw = parts.next().ok_or(ParseError::InsufficientParts)?;
        let part_of_speech = PartOfSpeech::try_from(part_of_speech_raw)?;
        morphemes.push(Morpheme {
            raw: raw.to_string(),
            part_of_speech,
        });
    }
    Ok(Sentence { morphemes })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_sentence_works_for_single_words() {
        let sentence = "猫";
        let raw = String::from("猫");
        let part_of_speech = PartOfSpeech::Noun;
        let result = Sentence {
            morphemes: vec![Morpheme {
                raw,
                part_of_speech,
            }],
        };
        assert_eq!(Ok(result), parse_sentence(sentence));
    }

    #[test]
    fn parts_of_speech_can_be_parsed_from_display() {
        let parts_of_speech = [
            PartOfSpeech::Noun,
            PartOfSpeech::Verb,
            PartOfSpeech::Particle,
        ];
        for pos in &parts_of_speech {
            let round_trip = PartOfSpeech::try_from(format!("{}", pos).as_ref());
            assert_eq!(Ok(*pos), round_trip);
        }
    }
}
