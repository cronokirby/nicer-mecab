use mecab::Tagger;

pub fn parse_sentence(sentence: &str) -> String {
    let mut tagger = Tagger::new("");
    tagger.parse_nbest_init(sentence);
    tagger.next().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_sentence_works_for_single_words() {
        let sentence = "猫";
        let result = "猫\t名詞,一般,*,*,*,*,猫,ネコ,ネコ\nEOS\n";
        assert_eq!(result, parse_sentence(sentence));
    }
}
