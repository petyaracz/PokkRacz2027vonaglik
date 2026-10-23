# -- head -- #

# for every -m hit, decide from context (Left/Right of the KWIC concordance) whether the use is
# definite or indefinite conjugation.
#
# earlier versions of this script only ran the context check on lemmas hand-tagged as
# "object_capable" (dat/lemma_transitivity_decisions.tsv), skipping the rest on the assumption
# that a bare verb with no object-taking reading can never be definite. that assumption is wrong:
# a preverb can make an otherwise strictly intransitive verb take a definite accusative object
# ("alszik" never takes an object bare, but "átalszom a délutánt", "I sleep through the
# afternoon", is fully grammatical and definite -- *"átalszok a délutánt" is ungrammatical). if
# the preverb separates from the verb in the sentence (routine under negation, focus, or object
# fronting: "a délutánt alszom át"), the bare verb token shows up in a literal corpus search with
# a genuine accusative object sitting right next to it, and the old object_capable filter would
# have silently skipped it and miscounted it as indefinite. so: run the context check on every
# -m hit, for every lemma, with no upfront filtering by transitivity.
#
# dat/lemma_transitivity_decisions.tsv still matters for lemmas flagged EXCLUDE_data_quality
# (corpus noise, not a real reading of the lemma at all -- e.g. ellik's hits are almost all a
# mangled "ellen"/proper noun, not the verb) -- those are dropped entirely, not just marked
# indefinite.

setwd('~/Github/PokkRacz2027vonaglik/')

library(tidyverse)

# -- read -- #

samp = read_tsv('dat/ik_sample.tsv') |> mutate(suffix = replace_na(suffix, 'stem'))
conc = read_tsv('dat/concordance_mnsz2_tidy.tsv') |> mutate(row_id = row_number())  # stable id: some
# (Reference,Left,KWIC,Right,lemma) tuples are genuine duplicate corpus hits, not join-safe keys
decisions = read_tsv('dat/lemma_transitivity_decisions.tsv')

# -- def -- #

exclude_lemmas = decisions |>
  mutate(final_call = coalesce(na_if(your_call, ''), claude_call)) |>
  filter(str_detect(str_to_lower(final_call), 'exclude')) |>
  pull(lemma)

# verbs confirmed (by hand, against real hits, not just assumed) to never take a definite
# reading under any construction: the context heuristic can't always tell, so these are
# hardcoded past it rather than re-litigated on every rerun. tartozik governs dative +
# instrumental only ("tartozom valakinek valamivel"), never accusative; its false "definite"
# hits all turned out to be the regex mistaking a bare dative/allative noun that happens to end
# in -at/-et (kötet, csapat, kertészet...) for an accusative-marked one, a structural limitation
# of surface-form matching that a real morphological parse would be needed to fix properly.
# nézelődik/szerencsétlenkedik/érdeklődik/kínlódik/alakoskodik/lábatlankodik/közlekedik/
# hülyéskedik are oblique-governing even with a preverb attached (unlike alszik/szokik, whose
# preverbed forms do take accusative objects -- see the particle_gap preverb list below).
# igyekszik only ever takes an infinitival complement ("igyekszem megnézni"), never a direct
# object itself; its false "definite" hits are the complement's own object sitting right next to
# it ("igyekszem az embereket jól megnézni" -- the real definite verb, if any, is "megnézni"/
# "megnézem", not igyekszem). esik is idiom/oblique-only ("kétségbe esik", "jól esik") and never
# takes a direct accusative object; its false hits are an accusative noun belonging to an
# unrelated clause nearby, not to esik.
always_indefinite = c('tartozik', 'nézelődik', 'szerencsétlenkedik', 'érdeklődik', 'kínlódik',
                       'alakoskodik', 'lábatlankodik', 'közlekedik', 'hülyéskedik',
                       'igyekszik', 'esik')

pairs = samp |>
  filter(!lemma %in% exclude_lemmas) |>
  transmute(lemma, k_form = form, m_form = str_replace(form, 'k$', 'm'))

conc = conc |> mutate(KWIC_lc = str_to_lower(KWIC))

rows = conc |>
  filter(KWIC_lc %in% pairs$m_form, !lemma %in% exclude_lemmas)

# -- fun -- #

# same-sentence context only: text after the last sentence boundary marker, so an object from
# the previous sentence doesn't get attributed to this verb
cleanLeft = function(x) {
  x = replace_na(x, '')
  parts = str_split(x, '</s><s>|<s>|</s>')
  map_chr(parts, ~ tail(.x, 1))
}

# mirror of cleanLeft for the Right side: same-sentence text up to the next boundary marker
cleanRight = function(x) {
  x = replace_na(x, '')
  parts = str_split(x, '</s><s>|<s>|</s>')
  map_chr(parts, ~ head(.x, 1))
}

# -- run -- #

rows = rows |>
  mutate(
    left_clean = cleanLeft(Left),
    right_clean = cleanRight(Right),
    last_toks = str_extract(str_trim(left_clean), '(\\S+\\s+){0,3}\\S+$'),   # last 4 tokens before the verb
    first_toks = str_extract(str_trim(right_clean), '^\\S+(\\s+\\S+){0,3}')  # first 4 tokens after the verb
  ) |>
  mutate(
    # negation and a handful of other particles routinely intervene between object and verb
    # ("az újat nem iszom") without breaking the object-verb relationship. a separated preverb
    # ("alszom át", "viszem el") is also common right after the verb and doesn't itself signal
    # anything about definiteness, so it's allowed to intervene on the right side too
    particle_gap_end = '(\\s+(nem|is|már|még|csak|mindig))*\\s*$',
    particle_gap_start = '^(\\s*(nem|is|már|még|csak|mindig|át|végig|túl|ki|be|fel|le|meg|el|rá|oda|össze|szét|körül)\\s+)*',
    # an adjective can sit between the article and the noun ("a zöld azúrt")
    adj_gap = '(\\S+\\s+){0,2}',

    # accusative pronouns/demonstratives, checked on both sides of the verb. magamat added (1sg
    # reflexive accusative: "átverekszem magamat a tömegen"). bare "magam" deliberately excluded:
    # spot-checking showed it's overwhelmingly the emphatic nominative ("magam nem tartozom
    # Vashoz" = "I myself don't belong to Vas") rather than a truncated accusative, so it does
    # more harm (false positives) than good here. mit/kit removed from here: interrogative
    # pronouns grammatically can NEVER license definite conjugation ("mit eszel", not "mit
    # eszed"), so they are a strong INdefinite signal, not a definite one -- see flag_interrog
    # below, which overrides everything else when it fires
    flag_pronoun_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(azt|ezt|őt|ugyanazt|mindezt|mindazt|magamat)\\b', particle_gap_end)), FALSE),
    flag_pronoun_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(azt|ezt|őt|ugyanazt|mindezt|mindazt|magamat)\\b')), FALSE),

    # interrogative, relative, and indefinite accusative pronouns (mit/kit "what/whom", amit/akit
    # "which/who [relative]", amelyet/amelyeket the formal-register "amely" sibling of amit,
    # valamit/valakit/semmit/senkit/mindenkit/egyet "something/someone/nothing/nobody/everyone/one
    # [of them]") force indefinite regardless of any other signal: none of these can co-occur with
    # definite conjugation (they're all non-specific referents), so an -ik verb's -m form here is
    # a genuine indefinite token (via the -k/-m levelling this whole project studies), not
    # evidence of definiteness. getting this backwards for mit/kit would have thrown away exactly
    # the indefinite-m data points the project most wants to keep. amelyet/amelyeket added after
    # a confirmed miss: "fürdöm ... amelyeket kellő nyájassággal fogadok" -- the accusative
    # belongs to the relative clause's own verb (fogadok), not to fürdöm
    flag_interrog_L = replace_na(str_detect(str_to_lower(last_toks), '\\b(mit|kit|amit|akit|amelyet|amelyeket|valamit|valakit|semmit|senkit|mindenkit|egyet)\\b\\s*$'), FALSE),
    flag_interrog_R = replace_na(str_detect(str_to_lower(first_toks), '^\\s*(mit|kit|amit|akit|amelyet|amelyeket|valamit|valakit|semmit|senkit|mindenkit|egyet)\\b'), FALSE),

    # sentence-initial discourse connectives that happen to end in vowel+t (sőt "moreover", tehát
    # "therefore", hát "well") and bare quantity/degree accusative ADVERBIALS (sokat "a lot",
    # kicsit "a little", nagyot "greatly", mélyeket "deeply", hangosat "loudly", keveset
    # "little", ennyit/annyit "this/that much", rengeteget "a ton") don't correlate with
    # definite conjugation at all despite ending in the same shape; this stoplist is necessarily
    # incomplete (degree-adverbial formation from adjectives is open-class), a known limitation
    capital_stop = '(sőt|tehát|hát|sokat|kicsit|nagyot|mélyeket|mélyet|hangosat|keveset|ennyit|annyit|rengeteget)',
    flag_capital_stop_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b', capital_stop, '\\b\\s*$')), FALSE),
    flag_capital_stop_R = replace_na(str_detect(str_to_lower(first_toks), paste0('^\\s*', capital_stop, '\\b')), FALSE),

    # words that end in vowel+t but can never be the article's accusative-marked noun:
    # capital_stop's degree adverbials plus "legtöbbet" (missing from that list), the frequency
    # adverb ismét "again", common nouns/loanwords whose BARE form coincidentally ends in -At
    # (saját "own", üzlet "shop", szupermarket), and the interrogative/relative/indefinite
    # pronouns from flag_interrog below (mit/kit/amit/akit/amelyet/amelyeket/valamit/valakit/
    # semmit/senkit/mindenkit/egyet) -- excluded via a negative lookahead positioned exactly
    # where flag_art_acc's own match lands (after the a/az + adj_gap), rather than reusing
    # flag_capital_stop_L/R, which are anchored to the very start of the window and so miss any
    # case where the excluded word isn't the first token ("a legtöbbet" with the article first,
    # "a hajlongásokban , amelyeket" with two words in between -- confirmed misses without this)
    art_acc_stop = '(sőt|tehát|hát|sokat|kicsit|nagyot|mélyeket|mélyet|hangosat|keveset|ennyit|annyit|rengeteget|legtöbbet|ismét|saját|üzlet|szupermarket|mit|kit|amit|akit|amelyet|amelyeket|valamit|valakit|semmit|senkit|mindenkit|egyet)',
    flag_art_acc_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(a|az)\\s+', adj_gap, '(?!\\b', art_acc_stop, '\\b)\\S*[aeiouáéíóöőúüű]t\\b', particle_gap_end)), FALSE),
    flag_art_acc_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(a|az)\\s+', adj_gap, '(?!\\b', art_acc_stop, '\\b)\\S*[aeiouáéíóöőúüű]t\\b')), FALSE),

    # bare common-noun accusatives, with no article and no possessive marker, do NOT trigger
    # definite conjugation ("Bokrot ugrom" = "I jump [a] bush" is indefinite, confirmed by hand)
    # even when capitalized purely for being sentence-initial ("Vágyat sugárzom" is the same
    # case: "vágy" = desire, bare, no article -> indefinite, not a real counterexample). so
    # capitalization alone is not a safe definiteness signal; split into two narrower mechanisms:

    # (1) possessive-accusative is always definite regardless of article or capitalization
    # ("ÁFÁját bukom" = "I forfeit its VAT-ACC", 3sg possessive "-ja/-je" + accusative "-t").
    # restricted to the "j" + vowel + t shape specifically: the "j" linking consonant is close to
    # diagnostic for the 3sg possessive suffix and rarely a coincidental bare-noun ending, unlike
    # bare -át/-ét (which real common nouns also end in, e.g. "kötet", "csapat", see the tartozik
    # false positives above) -- the possessive case without "j" (álmát, napját's sibling "napját"
    # itself has a j actually, "almát" the bare noun vs "álmát" the possessive differ only in
    # vowel length) is left to the narrow alszik-specific patch below, not generalized here
    # no leading \\b before "j": the linking consonant sits mid-word (áfá-JÁT), never at a word
    # start, so a leading boundary there would never match anything
    flag_poss_acc_L = replace_na(str_detect(str_to_lower(last_toks), paste0('j[áé]t\\b', particle_gap_end)), FALSE),
    flag_poss_acc_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, 'j[áé]t\\b')), FALSE),

    # (2) proper nouns are inherently definite even bare ("Juszufot fotózom" = "I photograph
    # Juszuf"). on the RIGHT side this is structurally safe without any position-checking: the
    # verb token always occupies the slot before it in the same sentence, so anything after the
    # verb can never be sentence-initial, so capitalization there reliably signals a real proper
    # noun. the LEFT side has NO reliable version of this: a sentence-initial-vs-mid-sentence
    # check was tried and abandoned -- it's defeated by punctuation before the capitalized word
    # (quote marks, dashes in dialogue: "Bokrot" preceded by \"\"\"\" still reads as
    # "sentence-initial" to a naive check and wrongly stays flagged definite), and even a working
    # version would be checking the wrong thing anyway, since "Bokrot" (bare common noun "bush",
    # confirmed indefinite) and "Nitzsét" (proper noun "Nietzsche", confirmed definite) are BOTH
    # sentence-initial in their actual corpus hits -- position cannot tell them apart, only real
    # named-entity knowledge can. so: no general left-side proper-noun pattern. the one left-side
    # case actually confirmed (Nitzsét + bújik) is handled as a narrow lemma-specific patch
    # instead, the same way álmot is handled for alszik two patches down
    flag_propn_acc_R = replace_na(str_detect(first_toks, paste0(particle_gap_start, '\\b[A-ZÁÉÍÓÖŐÚÜŰ]\\S*[aeiouáéíóöőúüűAEIOUÁÉÍÓÖŐÚÜŰ]t\\b')), FALSE),
    flag_nitzschet_L = replace_na(str_detect(str_to_lower(last_toks), paste0('(nitzsét|nietzschét)\\b', particle_gap_end)), FALSE),

    flag_capital_acc_L = (flag_poss_acc_L | flag_nitzschet_L) & !flag_capital_stop_L,
    flag_capital_acc_R = (flag_poss_acc_R | flag_propn_acc_R) & !flag_capital_stop_R,

    # -ért ("for the sake of") ends in t and would otherwise be caught by flag_art_acc above,
    # but it's a causal-final complement, not an accusative object. postpositions like "alatt"
    # ("under") end in a double -tt, so the vowel+t requirement above already excludes them
    # (confirmed with a substitution test: swap the -m verb for its 3sg definite form and check
    # the result reads as plausible Hungarian. "a hid alatt alussza" needs an object that "alatt"
    # can't provide, so it's implausible, and it doesn't match the pattern either, good)
    flag_ert_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(a|az)?\\s*', adj_gap, '\\S*ért\\b', particle_gap_end)), FALSE),
    flag_ert_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(a|az)?\\s*', adj_gap, '\\S*ért\\b')), FALSE),

    # "a fenét" ("the heck/hell!", fixed vulgar exclamatory idiom) grammatically looks like
    # article + accusative noun but isn't a real object, same treatment as -ért
    flag_fenet_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b(a|az)\\s+fen[ée]t\\b', particle_gap_end)), FALSE),
    flag_fenet_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b(a|az)\\s+fen[ée]t\\b')), FALSE),

    # accusative-governing converbs/postpositions ("X-et illetően/tekintve/nézve" = "regarding/
    # concerning X") govern their own accusative complement; it isn't the -m verb's object even
    # though it sits right next to it ("az összegeket illetően esem" -- esik never takes an
    # object at all, "összegeket" belongs to "illetően", not to esem). same treatment as -ért
    flag_illetoen_L = replace_na(str_detect(str_to_lower(last_toks), '\\b(illetően|illetoen|tekintve|nézve)\\b'), FALSE),
    flag_illetoen_R = replace_na(str_detect(str_to_lower(first_toks), '^\\s*(illetően|illetoen|tekintve|nézve)\\b'), FALSE),

    # "azt mond(j)a/mondják/mondta/mondták" is a fixed quotative formula: "azt" is cataphoric to
    # the reported clause that follows, not the preceding -m verb's object ("kérdezősködöm [...]
    # azt mondják [hogy ...]" -- kérdezősködik governs -ról/-ről, never accusative, and "azt"
    # belongs to "mondják", not to kérdezősködöm)
    flag_azt_mond_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\bazt\\s+mond')), FALSE),

    # alszik's most common transitive idiom ("alussza az almat" / "almat alszik", sleeps one's
    # sleep/dream) is a possessive-marked object with no article at all, so the flag_art_acc
    # pattern above can never catch it. narrow, lemma-specific patch rather than a general fix
    # for unarticled possessive objects, which would need real morphological analysis
    flag_almot_L = replace_na(str_detect(str_to_lower(last_toks), paste0('\\b\u00e1lm\\w*[a\u00e1]t\\b', particle_gap_end)), FALSE),
    flag_almot_R = replace_na(str_detect(str_to_lower(first_toks), paste0(particle_gap_start, '\\b\u00e1lm\\w*[a\u00e1]t\\b')), FALSE),

    flag_definite = (flag_pronoun_L | flag_art_acc_L | flag_pronoun_R | flag_art_acc_R | flag_almot_L | flag_almot_R | flag_capital_acc_L | flag_capital_acc_R),
    flag_false_positive = (flag_ert_L | flag_ert_R | flag_fenet_L | flag_fenet_R | flag_illetoen_L | flag_illetoen_R | flag_azt_mond_R),
    flag_interrog = (flag_interrog_L | flag_interrog_R),
    heuristic_call = if_else(flag_interrog, 'indefinite',
                      if_else(flag_definite & !flag_false_positive & !lemma %in% always_indefinite, 'definite', 'indefinite'))
  )

# known remaining gaps, left unfixed as low-prevalence (see readme): possessive-marked definites
# with no article at all ("napját álmodom"), and cross-clause pronoun misattribution in parallel
# constructions ("amit iszom, azt eszem is")

# -- write -- #

write_tsv(rows |> select(row_id, lemma, Reference, Left, KWIC, Right, heuristic_call), 'dat/objcap_rows_classified.tsv')
