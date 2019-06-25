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
    /// The specific usage couldn't be recognized
    UnknownUsage(String),
    /// The specific part of speech couldn't be recognized
    UnknownSpecificPOS(String),
    /// The verb column couldn't be recognized
    UnknownVerbColumn(String),
    /// The verb type couldn't be identified
    UnknownVerbType(String),
    /// The verb form couldn't be identified
    UnknownVerbForm(String),
}

// This is useful for parsing the optional fields given by mecab
fn from_asterisk<'a, T: TryFrom<&'a str>>(value: &'a str) -> Result<Option<T>, T::Error> {
    if value == "*" {
        Ok(None)
    } else {
        T::try_from(value).map(Some)
    }
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
    /// 記号, Punctuation, such as "。".
    Punctuation,
}

impl fmt::Display for PartOfSpeech {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            PartOfSpeech::Noun => "名詞",
            PartOfSpeech::Verb => "動詞",
            PartOfSpeech::Particle => "助詞",
            PartOfSpeech::Punctuation => "記号",
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
            "記号" => Ok(PartOfSpeech::Punctuation),
            _ => Err(ParseError::UnknownPartOfSpeech(value.into())),
        }
    }
}

/// Represents more specific information about how a morpheme is used in a sentence.
///
/// For example, this specifies the difference between a particle used as part of a
/// verb's conjugation, and a particle marking the role of a noun in a sentence.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Usage {
    /// 一般, Used with nouns, indicates an ordinary noun as opposed to proper nouns
    ///
    /// Can also be used for self explaining punctuation, like "？".
    General,
    /// 格助詞, Case marking particle, e.g. "が、は、に".
    CaseMarking,
    /// 接続助詞, Conjunction particle, e.g. "と"
    Conjunction,
    /// 自立, Independent, used for verbs that aren't a part of a conjugation.
    IndependentVerb,
    /// 固有名詞, Proper noun, used for nouns like places or names.
    ProperNoun,
    /// 連体化, Attribution, used for "の".
    Attribution,
    /// 句点, Period, with punctuation indicates the sentence ending "。".
    Period,
}

impl fmt::Display for Usage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            Usage::General => "一般",
            Usage::CaseMarking => "格助詞",
            Usage::Conjunction => "接続助詞",
            Usage::IndependentVerb => "自立",
            Usage::ProperNoun => "固有名詞",
            Usage::Attribution => "連体化",
            Usage::Period => "句点",
        };
        write!(f, "{}", jp)
    }
}

impl TryFrom<&str> for Usage {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "一般" => Ok(Usage::General),
            "格助詞" => Ok(Usage::CaseMarking),
            "自立" => Ok(Usage::IndependentVerb),
            "接続助詞" => Ok(Usage::Conjunction),
            "固有名詞" => Ok(Usage::ProperNoun),
            "連体化" => Ok(Usage::Attribution),
            "句点" => Ok(Usage::Period),
            _ => Err(ParseError::UnknownUsage(value.into())),
        }
    }
}

/// This provides more information about what specific part of speech a morpheme has.
///
/// For example, this allows us to distinguish between proper place names, and
/// proper person names. If the usage is general, this enum isn't relevant.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SpecificPOS {
    /// 一般, This indicates that no further information exists for the specific usage.
    ///
    /// This will be given with particles and pronouns, for example. In general, this
    /// comes when the usage is not general, but there's no extra information about
    /// the part of speech.
    General,
    /// 人名, This indicates a noun related to people.
    ///
    /// This can be used for pronouns like "私", as well as names like "田中".
    PersonalName,
    /// 地域, This indicates that the word is used for a region, e.g. "東京".
    Region,
}

impl fmt::Display for SpecificPOS {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            SpecificPOS::General => "一般",
            SpecificPOS::PersonalName => "人名",
            SpecificPOS::Region => "地域",
        };
        write!(f, "{}", jp)
    }
}

impl TryFrom<&str> for SpecificPOS {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "一般" => Ok(SpecificPOS::General),
            "人名" => Ok(SpecificPOS::PersonalName),
            "地域" => Ok(SpecificPOS::Region),
            _ => Err(ParseError::UnknownSpecificPOS(value.into())),
        }
    }
}

/// This holds more information about a name.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NameType {
    /// 一般, This indicates that no specific name type exists for this morpheme.
    General,
    /// 姓, Used for a person's family name.
    FamilyName,
    /// 名, Used for a person's first name.
    FirstName,
}

impl fmt::Display for NameType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            NameType::General => "一般",
            NameType::FamilyName => "姓",
            NameType::FirstName => "名",
        };
        write!(f, "{}", jp)
    }
}

impl TryFrom<&str> for NameType {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "一般" => Ok(NameType::General),
            "姓" => Ok(NameType::FamilyName),
            "名" => Ok(NameType::FirstName),
            _ => Err(ParseError::UnknownSpecificPOS(value.into())),
        }
    }
}

/// This represents a column in the hiragana table identifying a verb conjugation.
///
/// Informally called "u-verbs", these verbs have endings corresponding to a specific
/// column of the Japanese syllabary. For example, verbs in the "ka" column, end in "ku",
/// e.g. "聞く".
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VerbColumn {
    A,
    Ka,
    Ga,
    Sa,
    Ta,
    Na,
    Ba,
    Ma,
    Ra,
}

impl fmt::Display for VerbColumn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            VerbColumn::A => "ア",
            VerbColumn::Ka => "カ",
            VerbColumn::Ga => "ガ",
            VerbColumn::Sa => "サ",
            VerbColumn::Ta => "タ",
            VerbColumn::Na => "ナ",
            VerbColumn::Ba => "バ",
            VerbColumn::Ma => "マ",
            VerbColumn::Ra => "ラ",
        };
        write!(f, "{}", jp)
    }
}

impl TryFrom<&str> for VerbColumn {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "ア" => Ok(VerbColumn::A),
            "カ" => Ok(VerbColumn::Ka),
            "ガ" => Ok(VerbColumn::Ga),
            "サ" => Ok(VerbColumn::Sa),
            "タ" => Ok(VerbColumn::Ta),
            "ナ" => Ok(VerbColumn::Na),
            "バ" => Ok(VerbColumn::Ba),
            "マ" => Ok(VerbColumn::Ma),
            "ラ" => Ok(VerbColumn::Ra),
            _ => Err(ParseError::UnknownVerbColumn(value.into())),
        }
    }
}

/// This represents what type of verb this is.
///
/// This differentiates between regular verbs, and irregular verbs.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VerbType {
    /// 一段, The standard "ru verb" conjugation such as "食べる"
    Ichidan,
    /// 五段, The "u verb" conjugation such as "聞く"
    Godan(VerbColumn),
    /// The verb "来る" gets its own classification
    Kuru,
}

impl fmt::Display for VerbType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VerbType::Ichidan => write!(f, "一段"),
            VerbType::Godan(col) => write!(f, "五段・{}行", col),
            VerbType::Kuru => write!(f, "カ変・来ル"),
        }
    }
}

impl TryFrom<&str> for VerbType {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let godan = "五段・";
        if value.starts_with(godan) {
            return VerbColumn::try_from(&value[9..12]).map(VerbType::Godan);
        }
        match value {
            "一段" => Ok(VerbType::Ichidan),
            "カ変・来ル" => Ok(VerbType::Kuru),
            _ => Err(ParseError::UnknownVerbColumn(value.into())),
        }
    }
}

/// Represents the form a verb takes on in a sentence.
///
/// This is used to distinguish a plain verb from a verb stem, for example.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VerbForm {
    /// 基本形, The fundamental form for a verb, e.g. "見る"
    Fundamental,
}

impl fmt::Display for VerbForm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let jp = match *self {
            VerbForm::Fundamental => "基本形",
        };
        write!(f, "{}", jp)
    }
}

impl TryFrom<&str> for VerbForm {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "基本形" => Ok(VerbForm::Fundamental),
            _ => Err(ParseError::UnknownVerbForm(value.into())),
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
    /// This holds more specific information about how this morpheme is used.
    pub usage: Usage,
    /// If given, this holds specific information about the part of speech in general.
    pub specific_pos: Option<SpecificPOS>,
    /// If given, this holds information about what type of name this morpheme is.
    pub name_type: Option<NameType>,
    /// If given, this holds information about what type of conjugation this verb has.
    pub verb_type: Option<VerbType>,
    /// If given, this holds information about what type of form this verb takes on.
    pub verb_form: Option<VerbForm>,
    /// This contains the root form of the morpheme.
    ///
    /// For example, a conjugated verb will show its canonical form, e.g. "見る"
    pub root_form: String,
    /// A katakana representation of the syllables forming the morpheme.
    pub reading: String,
    /// A katakana representation of the pronunciation forming the morpheme.
    ///
    /// The difference between this field and the last one, is how long vowels are presented.
    /// In this field, long vowels are represented using a "ー", e.g. "トーキョー", instead
    /// of "トウキョウ" like the reading field
    pub pronunciation: String,
}

/// This represents the information parsed by the analyzer.
#[derive(Clone, Debug, PartialEq)]
pub struct Sentence {
    /// The list of morphemes composing the sentence.
    pub morphemes: Vec<Morpheme>,
}

impl Sentence {
    pub fn parse(sentence: &str) -> Result<Self, ParseError> {
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
            let usage_raw = parts.next().ok_or(ParseError::InsufficientParts)?;
            let usage = Usage::try_from(usage_raw)?;
            let specific_pos_raw = parts.next().ok_or(ParseError::InsufficientParts)?;
            let specific_pos = from_asterisk(specific_pos_raw)?;
            let name_type_raw = parts.next().ok_or(ParseError::InsufficientParts)?;
            let name_type = from_asterisk(name_type_raw)?;
            let verb_type_raw = parts.next().ok_or(ParseError::InsufficientParts)?;
            let verb_type = from_asterisk(verb_type_raw)?;
            let verb_form_raw = parts.next().ok_or(ParseError::InsufficientParts)?;
            let verb_form = from_asterisk(verb_form_raw)?;
            let root_form = parts.next().ok_or(ParseError::InsufficientParts)?.into();
            let reading = parts.next().ok_or(ParseError::InsufficientParts)?.into();
            let pronunciation = parts.next().ok_or(ParseError::InsufficientParts)?.into();
            morphemes.push(Morpheme {
                raw: raw.to_string(),
                part_of_speech,
                usage,
                specific_pos,
                name_type,
                verb_type,
                verb_form,
                root_form,
                reading,
                pronunciation,
            });
        }
        Ok(Sentence { morphemes })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_sentence_works_for_an_example_sentence() {
        let sentence = "東京に村上春樹の猫がいると走る。";
        let result = Sentence {
            morphemes: vec![
                Morpheme {
                    raw: "東京".into(),
                    part_of_speech: PartOfSpeech::Noun,
                    usage: Usage::ProperNoun,
                    specific_pos: Some(SpecificPOS::Region),
                    name_type: Some(NameType::General),
                    verb_type: None,
                    verb_form: None,
                    root_form: "東京".into(),
                    reading: "トウキョウ".into(),
                    pronunciation: "トーキョー".into(),
                },
                Morpheme {
                    raw: "に".into(),
                    part_of_speech: PartOfSpeech::Particle,
                    usage: Usage::CaseMarking,
                    specific_pos: Some(SpecificPOS::General),
                    name_type: None,
                    verb_type: None,
                    verb_form: None,
                    root_form: "に".into(),
                    reading: "ニ".into(),
                    pronunciation: "ニ".into(),
                },
                Morpheme {
                    raw: "村上".into(),
                    part_of_speech: PartOfSpeech::Noun,
                    usage: Usage::ProperNoun,
                    specific_pos: Some(SpecificPOS::PersonalName),
                    name_type: Some(NameType::FamilyName),
                    verb_type: None,
                    verb_form: None,
                    root_form: "村上".into(),
                    reading: "ムラカミ".into(),
                    pronunciation: "ムラカミ".into(),
                },
                Morpheme {
                    raw: "春樹".into(),
                    part_of_speech: PartOfSpeech::Noun,
                    usage: Usage::ProperNoun,
                    specific_pos: Some(SpecificPOS::PersonalName),
                    name_type: Some(NameType::FirstName),
                    verb_type: None,
                    verb_form: None,
                    root_form: "春樹".into(),
                    reading: "ハルキ".into(),
                    pronunciation: "ハルキ".into(),
                },
                Morpheme {
                    raw: "の".into(),
                    part_of_speech: PartOfSpeech::Particle,
                    usage: Usage::Attribution,
                    specific_pos: None,
                    name_type: None,
                    verb_type: None,
                    verb_form: None,
                    root_form: "の".into(),
                    reading: "ノ".into(),
                    pronunciation: "ノ".into(),
                },
                Morpheme {
                    raw: "猫".into(),
                    part_of_speech: PartOfSpeech::Noun,
                    usage: Usage::General,
                    specific_pos: None,
                    name_type: None,
                    verb_type: None,
                    verb_form: None,
                    root_form: "猫".into(),
                    reading: "ネコ".into(),
                    pronunciation: "ネコ".into(),
                },
                Morpheme {
                    raw: "が".into(),
                    part_of_speech: PartOfSpeech::Particle,
                    usage: Usage::CaseMarking,
                    specific_pos: Some(SpecificPOS::General),
                    name_type: None,
                    verb_type: None,
                    verb_form: None,
                    root_form: "が".into(),
                    reading: "ガ".into(),
                    pronunciation: "ガ".into()
                },
                Morpheme {
                    raw: "いる".into(),
                    part_of_speech: PartOfSpeech::Verb,
                    usage: Usage::IndependentVerb,
                    specific_pos: None,
                    name_type: None,
                    verb_type: Some(VerbType::Ichidan),
                    verb_form: Some(VerbForm::Fundamental),
                    root_form: "いる".into(),
                    reading: "イル".into(),
                    pronunciation: "イル".into()
                },
                Morpheme {
                    raw: "と".into(),
                    part_of_speech: PartOfSpeech::Particle,
                    usage: Usage::Conjunction,
                    specific_pos: None,
                    name_type: None,
                    verb_type: None,
                    verb_form: None,
                    root_form: "と".into(),
                    reading: "ト".into(),
                    pronunciation: "ト".into()
                },
                Morpheme {
                    raw: "走る".into(),
                    part_of_speech: PartOfSpeech::Verb,
                    usage: Usage::IndependentVerb,
                    specific_pos: None,
                    name_type: None,
                    verb_type: Some(VerbType::Godan(VerbColumn::Ra)),
                    verb_form: Some(VerbForm::Fundamental),
                    root_form: "走る".into(),
                    reading: "ハシル".into(),
                    pronunciation: "ハシル".into()
                },
                Morpheme {
                    raw: "。".into(),
                    part_of_speech: PartOfSpeech::Punctuation,
                    usage: Usage::Period,
                    specific_pos: None,
                    name_type: None,
                    verb_type: None,
                    verb_form: None,
                    root_form: "。".into(),
                    reading: "。".into(),
                    pronunciation: "。".into()
                },
            ],
        };
        assert_eq!(Ok(result), Sentence::parse(sentence));
    }

    #[test]
    fn parse_sentence_is_ok_for_kokoro() {
        let kokoro = include_str!("../sample-texts/kokoro.txt");
        for sentence in kokoro.lines() {
            let res = Sentence::parse(sentence);
            if !res.is_ok() {
                println!("{}\n{:?}", sentence, res);
            }
            assert!(Sentence::parse(sentence).is_ok());
        }
    }

    #[test]
    fn parts_of_speech_can_be_parsed_from_display() {
        let parts_of_speech = [
            PartOfSpeech::Noun,
            PartOfSpeech::Verb,
            PartOfSpeech::Particle,
            PartOfSpeech::Punctuation,
        ];
        for pos in &parts_of_speech {
            let round_trip = PartOfSpeech::try_from(format!("{}", pos).as_ref());
            assert_eq!(Ok(*pos), round_trip);
        }
    }

    #[test]
    fn usage_can_be_parsed_from_display() {
        let usages = [
            Usage::General,
            Usage::CaseMarking,
            Usage::Conjunction,
            Usage::IndependentVerb,
            Usage::ProperNoun,
            Usage::Attribution,
            Usage::Period,
        ];
        for usage in &usages {
            let round_trip = Usage::try_from(format!("{}", usage).as_ref());
            assert_eq!(Ok(*usage), round_trip);
        }
    }

    #[test]
    fn specific_pos_can_be_parsed_from_display() {
        let specific_pos = [
            SpecificPOS::General,
            SpecificPOS::PersonalName,
            SpecificPOS::Region,
        ];
        for s in &specific_pos {
            let round_trip = SpecificPOS::try_from(format!("{}", s).as_ref());
            assert_eq!(Ok(*s), round_trip);
        }
    }

    #[test]
    fn name_type_can_be_parsed_from_display() {
        let name_types = [NameType::General, NameType::FamilyName, NameType::FirstName];
        for name in &name_types {
            let round_trip = NameType::try_from(format!("{}", name).as_ref());
            assert_eq!(Ok(*name), round_trip);
        }
    }

    #[test]
    fn verb_type_can_be_parsed_from_display() {
        let verb_types = [
            VerbType::Ichidan,
            VerbType::Godan(VerbColumn::A),
            VerbType::Godan(VerbColumn::Ka),
            VerbType::Godan(VerbColumn::Ga),
            VerbType::Godan(VerbColumn::Ma),
            VerbType::Godan(VerbColumn::Sa),
            VerbType::Godan(VerbColumn::Ta),
            VerbType::Godan(VerbColumn::Na),
            VerbType::Godan(VerbColumn::Ra),
            VerbType::Godan(VerbColumn::Ba),
            VerbType::Kuru,
        ];
        for v in &verb_types {
            let round_trip = VerbType::try_from(format!("{}", v).as_ref());
            assert_eq!(Ok(*v), round_trip);
        }
    }

    #[test]
    fn verb_form_can_be_parsed_from_display() {
        let verb_forms = [VerbForm::Fundamental];
        for v in &verb_forms {
            let round_trip = VerbForm::try_from(format!("{}", v).as_ref());
            assert_eq!(Ok(*v), round_trip);
        }
    }
}
