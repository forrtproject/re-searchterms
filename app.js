/* ─────────────────────────────────────────────────────────────
   v9 full form localization layer
   Keeps stored values in English/stable machine-readable values,
   but displays labels, dropdown options, tag text, helper text and
   validation messages in the selected definition language.
   ───────────────────────────────────────────────────────────── */
const FULL_FORM_I18N = {
  en: {
    langOptions:{en:'English',de:'German', 'zh-hans':'Chinese Simplified', 'zh-hant':'Chinese Traditional', other:'Other'},
    select:'Select...', skip:'Skip', unsure:'Not sure / skip', noStatus:'No status suggestion', check:'Check', submitDef:'Submit definition for review', submitAnno:'Submit annotation', remove:'Remove',
    labels:{
      'Contribute a definition':'Contribute a definition','Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.':'Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.',
      'Term being defined':'Term being defined','Term label or synonym used by the source':'Term label or synonym used by the source','Language of submitted definition':'Language of submitted definition','Definition wording type':'Definition wording type','Definition text':'Definition text','Where can another person find this definition?':'Where can another person find this definition?','Locator type':'Locator type','Exact page / section / slide / entry':'Exact page / section / slide / entry','Source type':'Source type','Full citation':'Full citation','Author(s) of the current source':'Author(s) of the current source','Year':'Year','Title of current source':'Title of current source','Publication outlet / publisher / organisation':'Publication outlet / publisher / organisation','Access date':'Access date','Source accessibility':'Source accessibility','How does the current source present this definition?':'How does the current source present this definition?','Discipline(s)':'Discipline(s)','Research context(s)':'Research context(s)','Definition style':'Definition style','Scope of definition':'Scope of definition','Why might someone choose this definition?':'Why might someone choose this definition?','Community tags':'Community tags','Suitable context(s)':'Suitable context(s)','Confidence that the definition was copied/translated accurately':'Confidence that the definition was copied/translated accurately','Confidence that the source metadata are correct':'Confidence that the source metadata are correct','Definition being annotated':'Definition being annotated','Source verification':'Source verification','Source term label / synonym, if different':'Source term label / synonym, if different','Full citation or correction':'Full citation or correction','How does the source present this definition?':'How does the source present this definition?','Why might someone choose, avoid, or contextualise this definition?':'Why might someone choose, avoid, or contextualise this definition?','Annotation confidence':'Annotation confidence','Suggested source-check status':'Suggested source-check status','Tags':'Tags'
    },
    sections:{'1. DEFINITION INFORMATION':'1. DEFINITION INFORMATION','2. SOURCE INFORMATION':'2. SOURCE INFORMATION','3. DEFINITION PROVENANCE':'3. DEFINITION PROVENANCE','4. CONCEPTUAL CONTEXT AND ANNOTATION':'4. CONCEPTUAL CONTEXT AND ANNOTATION','5. CONTRIBUTOR CONFIDENCE':'5. CONTRIBUTOR CONFIDENCE','6. VERIFICATION CHECKLIST':'6. VERIFICATION CHECKLIST','1. SOURCE CHECK AND MISSING SOURCE METADATA':'1. SOURCE CHECK AND MISSING SOURCE METADATA','2. DEFINITION PROVENANCE':'2. DEFINITION PROVENANCE','3. CONCEPTUAL ANNOTATION':'3. CONCEPTUAL ANNOTATION'},
    definitionType:{exact_source_wording:'Exact wording as it appears in the source',source_provided_translation:'Translation provided by the source itself',contributor_translation_of_source:'Contributor translation of a source definition'},
    sourceLocationType:{'':'Select...',page:'Page number(s)',section:'Named section',chapter:'Chapter',slide:'Slide number',glossary_entry:'Glossary entry',dictionary_entry:'Dictionary entry',appendix:'Appendix',paragraph:'Paragraph number',table_or_figure:'Table or figure',other:'Other locator',entry:'Dictionary / glossary entry'},
    sourceType:{'':'Skip',journal_article:'Journal article',book:'Book',book_chapter:'Book chapter',textbook:'Textbook',dictionary:'Dictionary',glossary:'Glossary',website:'Website',report:'Report',policy_document:'Policy document',conference_paper:'Conference paper',slides:'Lecture / workshop slides',teaching_material:'Teaching material',other:'Other'},
    sourceAccessibility:{'':'Select...',open_access:'Open access',institutional_access:'Institutional access required',physical_copy:'Physical copy only',personal_copy:'Personal copy',unknown:'Unknown'},
    provenance:{'':'Select...',original_to_current_source:'The current source appears to propose this definition itself',direct_quote_from_cited_source:'The current source directly quotes an earlier cited source',adapted_from_cited_source:'The current source adapts or paraphrases an earlier cited source',synthesises_multiple_cited_sources:'The current source combines or summarises multiple earlier cited sources',provenance_unclear:'Unclear / no explicit provenance given'},
    style:{'':'Not sure / skip',theoretical:'Theoretical — explains what the concept means',operational:'Operational — specifies how the concept is measured or identified',normative:'Normative — states what should be done or valued',descriptive:'Descriptive — describes common usage or features',procedural:'Procedural — describes steps, practices, or processes',educational:'Educational — written for teaching or learning',policy_oriented:'Policy-oriented — written for governance, regulation, or institutional use',other:'Other',unclear:'Unclear'},
    scope:{'':'Not sure / skip',very_broad:'Very broad',broad:'Broad',moderate:'Moderate',narrow:'Narrow',very_narrow:'Very narrow'},
    confidence:{'':'Select...',5:'5 — Very confident',4:'4 — Confident',3:'3 — Moderately confident',2:'2 — Somewhat uncertain',1:'1 — Uncertain'},
    status:{'':'No status suggestion',source_verified:'Source-verified: definition matches cited source',flagged:'Flagged for review',archived:'Archive / duplicate / superseded'},
    sourceCheck:{'':'Select...',definition_matches_source:'I checked — definition matches the cited source',definition_partly_matches_source:'I checked — partly matches / needs attention',definition_does_not_match_source:'I checked — does NOT match',source_not_accessible:'I could not access the source',not_checked:'I did not check the source'},
    disciplines:{psychology:'Psychology',medicine:'Medicine',education:'Education',linguistics:'Linguistics',neuroscience:'Neuroscience',philosophy:'Philosophy',statistics:'Statistics',computer_science:'Computer science',engineering:'Engineering',library_information_science:'Library & information science',sociology:'Sociology',political_science:'Political science',economics_business:'Economics / Business',communication_media:'Communication / Media studies',law:'Law / Legal studies',humanities:'Humanities',environmental_science:'Environmental science',social_science:'Social sciences',interdisciplinary:'Interdisciplinary',other:'Other'},
    contexts:{open_science:'Open science',research_methods:'Research methods',statistics:'Statistics',measurement:'Measurement',theory_development:'Theory development',research_design:'Research design',meta_research:'Meta-research',education:'Education',scientific_communication:'Scientific communication',research_ethics:'Research ethics',policy:'Policy',clinical_practice:'Clinical practice',industry:'Industry',student_learning:'Student learning',grant_writing:'Grant writing',public_communication:'Public communication',academic_research:'Academic research',other:'Other'},
    tags:{'discipline-specific':'Discipline-specific',discipline_specific:'Discipline-specific',contested:'Contested','emerging-concept':'Emerging concept',emerging_concept:'Emerging concept',historical:'Historical',outdated:'Outdated',ambiguous:'Ambiguous','jargon-heavy':'Jargon-heavy',jargon_heavy:'Jargon-heavy','accessible-to-non-experts':'Accessible to non-experts',accessible_to_non_experts:'Accessible to non-experts','overly-broad':'Overly broad',overly_broad:'Overly broad','overly-narrow':'Overly narrow',overly_narrow:'Overly narrow','frequently-used':'Frequently used',frequently_used:'Frequently used','rarely-used':'Rarely used',rarely_used:'Rarely used',normative:'Normative','value-laden':'Value-laden',value_laden:'Value-laden',possible_jingle_fallacy_same_label_different_meaning:'Possible jingle fallacy: same label, different meaning',possible_jangle_fallacy_different_label_similar_meaning:'Possible jangle fallacy: different label, similar meaning',term_used_differently_from_other_definitions:'Term used differently from other definitions',different_concept_under_same_term:'May refer to a different concept'},
    suitable:{academic_research:'Academic research',teaching:'Teaching',student_learning:'Student learning',grant_writing:'Grant writing',public_communication:'Public communication',policy:'Policy',clinical_practice:'Clinical practice',industry:'Industry',meta_research:'Meta-research'},
    checkRows:['I copied the definition exactly as it appears in the source, except where I explicitly marked it as a translation.','I kept the in-text citations that appear inside the definition, if any.','I confirm this is not my own synthesis, interpretation, or paraphrase.','I checked that the citation metadata are as accurate as possible.','I provided a page, section, slide, entry, or other locator so another person can verify the definition.','I understand that the submission may be reviewed, source-verified, flagged, or archived by project contributors.'],
    statusHelp:'<strong>Note:</strong> Source-verified only means that the wording and metadata appear to match the cited source. It does not mean this is a recommended, preferred, or conceptually superior definition.'
  }
};
FULL_FORM_I18N.de = {
  ...FULL_FORM_I18N.en,
  langOptions:{en:'Englisch',de:'Deutsch','zh-hans':'Chinesisch (vereinfacht)','zh-hant':'Chinesisch (traditionell)',other:'Andere'}, select:'Auswählen...', skip:'Überspringen', unsure:'Nicht sicher / überspringen', noStatus:'Kein Statusvorschlag', check:'Prüfen', submitDef:'Definition zur Prüfung einreichen', submitAnno:'Annotation einreichen', remove:'Entfernen',
  labels:{...FULL_FORM_I18N.en.labels,'Contribute a definition':'Definition beitragen','Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.':'Reichen Sie eine Definition aus einer überprüfbaren Quelle ein. Bitte bewahren Sie den Wortlaut der Quelle und übernehmen Sie Zitationen, die innerhalb der Definition erscheinen.','Term being defined':'Definierter Begriff','Term label or synonym used by the source':'In der Quelle verwendetes Begriffslabel oder Synonym','Language of submitted definition':'Sprache der eingereichten Definition','Definition wording type':'Art des Definitionswortlauts','Definition text':'Definitionstext','Where can another person find this definition?':'Wo kann eine andere Person diese Definition finden?','Locator type':'Art der Fundstelle','Exact page / section / slide / entry':'Exakte Seite / Abschnitt / Folie / Eintrag','Source type':'Quellentyp','Full citation':'Vollständige Zitation','Author(s) of the current source':'Autor:innen der aktuellen Quelle','Year':'Jahr','Title of current source':'Titel der aktuellen Quelle','Publication outlet / publisher / organisation':'Publikationsort / Verlag / Organisation','Access date':'Zugriffsdatum','Source accessibility':'Zugänglichkeit der Quelle','How does the current source present this definition?':'Wie stellt die aktuelle Quelle diese Definition dar?','Discipline(s)':'Disziplin(en)','Research context(s)':'Forschungskontext(e)','Definition style':'Definitionsstil','Scope of definition':'Umfang der Definition','Why might someone choose this definition?':'Warum könnte jemand diese Definition wählen?','Community tags':'Community-Tags','Suitable context(s)':'Geeignete Kontexte','Confidence that the definition was copied/translated accurately':'Sicherheit, dass die Definition korrekt kopiert/übersetzt wurde','Confidence that the source metadata are correct':'Sicherheit, dass die Quellenmetadaten korrekt sind','Definition being annotated':'Zu annotierende Definition','Source verification':'Quellenprüfung','Source term label / synonym, if different':'Quellenbegriff / Synonym, falls abweichend','Full citation or correction':'Vollständige Zitation oder Korrektur','How does the source present this definition?':'Wie stellt die Quelle diese Definition dar?','Why might someone choose, avoid, or contextualise this definition?':'Warum könnte jemand diese Definition wählen, vermeiden oder kontextualisieren?','Annotation confidence':'Sicherheit der Annotation','Suggested source-check status':'Vorgeschlagener Quellenprüfungsstatus','Tags':'Tags'},
  sections:{'1. DEFINITION INFORMATION':'1. DEFINITION','2. SOURCE INFORMATION':'2. QUELLENANGABEN','3. DEFINITION PROVENANCE':'3. HERKUNFT DER DEFINITION','4. CONCEPTUAL CONTEXT AND ANNOTATION':'4. KONZEPTUELLER KONTEXT UND ANNOTATION','5. CONTRIBUTOR CONFIDENCE':'5. SICHERHEIT DER BEITRAGENDEN','6. VERIFICATION CHECKLIST':'6. PRÜFCHECKLISTE','1. SOURCE CHECK AND MISSING SOURCE METADATA':'1. QUELLENPRÜFUNG UND FEHLENDE METADATEN','2. DEFINITION PROVENANCE':'2. HERKUNFT DER DEFINITION','3. CONCEPTUAL ANNOTATION':'3. KONZEPTUELLE ANNOTATION'},
  definitionType:{exact_source_wording:'Exakter Wortlaut aus der Quelle',source_provided_translation:'Übersetzung, die von der Quelle selbst bereitgestellt wird',contributor_translation_of_source:'Übersetzung einer Quelldefinition durch die beitragende Person'},
  sourceLocationType:{'':'Auswählen...',page:'Seitenzahl(en)',section:'Benannter Abschnitt',chapter:'Kapitel',slide:'Foliennummer',glossary_entry:'Glossareintrag',dictionary_entry:'Wörterbucheintrag',appendix:'Anhang',paragraph:'Absatznummer',table_or_figure:'Tabelle oder Abbildung',other:'Andere Fundstelle',entry:'Wörterbuch- / Glossareintrag'},
  sourceType:{'':'Überspringen',journal_article:'Zeitschriftenartikel',book:'Buch',book_chapter:'Buchkapitel',textbook:'Lehrbuch',dictionary:'Wörterbuch',glossary:'Glossar',website:'Website',report:'Bericht',policy_document:'Policy-Dokument',conference_paper:'Konferenzbeitrag',slides:'Vorlesungs- / Workshop-Folien',teaching_material:'Lehrmaterial',other:'Andere'},
  sourceAccessibility:{'':'Auswählen...',open_access:'Open Access',institutional_access:'Institutioneller Zugang erforderlich',physical_copy:'Nur physisches Exemplar',personal_copy:'Persönliches Exemplar',unknown:'Unbekannt'},
  provenance:{'':'Auswählen...',original_to_current_source:'Die aktuelle Quelle scheint diese Definition selbst vorzuschlagen',direct_quote_from_cited_source:'Die aktuelle Quelle zitiert eine frühere zitierte Quelle direkt',adapted_from_cited_source:'Die aktuelle Quelle adaptiert oder paraphrasiert eine frühere zitierte Quelle',synthesises_multiple_cited_sources:'Die aktuelle Quelle kombiniert oder fasst mehrere frühere zitierte Quellen zusammen',provenance_unclear:'Unklar / keine explizite Herkunft angegeben'},
  style:{'':'Nicht sicher / überspringen',theoretical:'Theoretisch — erklärt, was das Konzept bedeutet',operational:'Operational — legt fest, wie das Konzept gemessen oder identifiziert wird',normative:'Normativ — beschreibt, was getan oder wertgeschätzt werden sollte',descriptive:'Deskriptiv — beschreibt übliche Verwendung oder Merkmale',procedural:'Prozedural — beschreibt Schritte, Praktiken oder Prozesse',educational:'Didaktisch — für Lehre oder Lernen formuliert',policy_oriented:'Policy-orientiert — für Governance, Regulierung oder institutionelle Nutzung',other:'Andere',unclear:'Unklar'},
  scope:{'':'Nicht sicher / überspringen',very_broad:'Sehr breit',broad:'Breit',moderate:'Mittel',narrow:'Eng',very_narrow:'Sehr eng'}, confidence:{'':'Auswählen...',5:'5 — Sehr sicher',4:'4 — Sicher',3:'3 — Mäßig sicher',2:'2 — Etwas unsicher',1:'1 — Unsicher'}, status:{'':'Kein Statusvorschlag',source_verified:'Quellengeprüft: Definition stimmt mit der zitierten Quelle überein',flagged:'Zur Prüfung markiert',archived:'Archivieren / Duplikat / ersetzt'}, sourceCheck:{'':'Auswählen...',definition_matches_source:'Geprüft — Definition stimmt mit der zitierten Quelle überein',definition_partly_matches_source:'Geprüft — stimmt teilweise überein / benötigt Aufmerksamkeit',definition_does_not_match_source:'Geprüft — stimmt NICHT überein',source_not_accessible:'Ich konnte auf die Quelle nicht zugreifen',not_checked:'Ich habe die Quelle nicht geprüft'},
  disciplines:{...FULL_FORM_I18N.en.disciplines,psychology:'Psychologie',medicine:'Medizin',education:'Bildungswissenschaft',linguistics:'Linguistik',neuroscience:'Neurowissenschaft',philosophy:'Philosophie',statistics:'Statistik',computer_science:'Informatik',engineering:'Ingenieurwissenschaften',library_information_science:'Bibliotheks- und Informationswissenschaft',sociology:'Soziologie',political_science:'Politikwissenschaft',economics_business:'Wirtschaftswissenschaft / Business',communication_media:'Kommunikations- / Medienwissenschaft',law:'Rechtswissenschaft',humanities:'Geisteswissenschaften',environmental_science:'Umweltwissenschaft',social_science:'Sozialwissenschaften',interdisciplinary:'Interdisziplinär',other:'Andere'},
  contexts:{...FULL_FORM_I18N.en.contexts,open_science:'Open Science',research_methods:'Forschungsmethoden',measurement:'Messung',theory_development:'Theorieentwicklung',research_design:'Forschungsdesign',meta_research:'Metaforschung',scientific_communication:'Wissenschaftskommunikation',research_ethics:'Forschungsethik',clinical_practice:'Klinische Praxis',industry:'Industrie',student_learning:'Studentisches Lernen',grant_writing:'Antragstellung',public_communication:'Öffentliche Kommunikation',academic_research:'Akademische Forschung',other:'Andere'},
  tags:{...FULL_FORM_I18N.en.tags,'discipline-specific':'Disziplinspezifisch',discipline_specific:'Disziplinspezifisch',contested:'Umstritten','emerging-concept':'Entstehendes Konzept',emerging_concept:'Entstehendes Konzept',historical:'Historisch',outdated:'Veraltet',ambiguous:'Mehrdeutig','jargon-heavy':'Jargonlastig',jargon_heavy:'Jargonlastig','accessible-to-non-experts':'Für Nicht-Expert:innen zugänglich',accessible_to_non_experts:'Für Nicht-Expert:innen zugänglich','overly-broad':'Zu breit',overly_broad:'Zu breit','overly-narrow':'Zu eng',overly_narrow:'Zu eng','frequently-used':'Häufig verwendet',frequently_used:'Häufig verwendet','rarely-used':'Selten verwendet',rarely_used:'Selten verwendet',normative:'Normativ','value-laden':'Wertgeladen',value_laden:'Wertgeladen',possible_jingle_fallacy_same_label_different_meaning:'Mögliche Jingle-Fallacy: gleicher Begriff, andere Bedeutung',possible_jangle_fallacy_different_label_similar_meaning:'Mögliche Jangle-Fallacy: anderer Begriff, ähnliche Bedeutung',term_used_differently_from_other_definitions:'Begriff wird anders verwendet als in anderen Definitionen',different_concept_under_same_term:'Könnte ein anderes Konzept bezeichnen'},
  suitable:{academic_research:'Akademische Forschung',teaching:'Lehre',student_learning:'Studentisches Lernen',grant_writing:'Antragstellung',public_communication:'Öffentliche Kommunikation',policy:'Policy',clinical_practice:'Klinische Praxis',industry:'Industrie',meta_research:'Metaforschung'},
  checkRows:['Ich habe die Definition exakt aus der Quelle übernommen, außer wenn ich sie ausdrücklich als Übersetzung markiert habe.','Ich habe die In-Text-Zitationen beibehalten, die innerhalb der Definition erscheinen, falls vorhanden.','Ich bestätige, dass dies nicht meine eigene Synthese, Interpretation oder Paraphrase ist.','Ich habe die Zitationsmetadaten so genau wie möglich geprüft.','Ich habe eine Seite, einen Abschnitt, eine Folie, einen Eintrag oder eine andere Fundstelle angegeben, damit eine andere Person die Definition überprüfen kann.','Ich verstehe, dass die Einreichung von Projektmitwirkenden geprüft, quellengeprüft, markiert oder archiviert werden kann.'],
  statusHelp:'<strong>Hinweis:</strong> Quellengeprüft bedeutet nur, dass Wortlaut und Metadaten zur zitierten Quelle zu passen scheinen. Es bedeutet nicht, dass diese Definition empfohlen, bevorzugt oder konzeptuell überlegen ist.'
};
FULL_FORM_I18N['zh-hans'] = {
  ...FULL_FORM_I18N.en,
  langOptions:{en:'英语',de:'德语','zh-hans':'中文简体','zh-hant':'中文繁体',other:'其他'}, select:'请选择...', skip:'跳过', unsure:'不确定 / 跳过', noStatus:'不建议更改状态', check:'检查', submitDef:'提交定义以供审核', submitAnno:'提交注释', remove:'移除',
  labels:{...FULL_FORM_I18N.en.labels,'Contribute a definition':'提交定义','Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.':'请提交来自可核查来源的定义。请保留来源中的原文措辞，并保留定义内部出现的引用。','Term being defined':'被定义的术语','Term label or synonym used by the source':'来源中使用的术语标签或同义词','Language of submitted definition':'提交定义的语言','Definition wording type':'定义文本类型','Definition text':'定义文本','Where can another person find this definition?':'其他人在哪里可以找到这个定义？','Locator type':'定位类型','Exact page / section / slide / entry':'具体页码 / 章节 / 幻灯片 / 词条','Source type':'来源类型','Full citation':'完整引用','Author(s) of the current source':'当前来源的作者','Year':'年份','Title of current source':'当前来源标题','Publication outlet / publisher / organisation':'期刊 / 出版社 / 机构','Access date':'访问日期','Source accessibility':'来源可获取性','How does the current source present this definition?':'当前来源如何呈现这个定义？','Discipline(s)':'学科','Research context(s)':'研究语境','Definition style':'定义类型 / 风格','Scope of definition':'定义范围','Why might someone choose this definition?':'为什么有人会选用这个定义？','Community tags':'社区标签','Suitable context(s)':'适用语境','Confidence that the definition was copied/translated accurately':'对定义复制/翻译准确性的信心','Confidence that the source metadata are correct':'对来源元数据准确性的信心','Definition being annotated':'正在注释的定义','Source verification':'来源核查','Source term label / synonym, if different':'来源使用的术语标签 / 同义词（如不同）','Full citation or correction':'完整引用或修正','How does the source present this definition?':'该来源如何呈现这个定义？','Why might someone choose, avoid, or contextualise this definition?':'为什么有人会选用、避免或需要说明这个定义？','Annotation confidence':'注释信心','Suggested source-check status':'建议的来源核查状态','Tags':'标签'},
  sections:{'1. DEFINITION INFORMATION':'1. 定义信息','2. SOURCE INFORMATION':'2. 来源信息','3. DEFINITION PROVENANCE':'3. 定义来源脉络','4. CONCEPTUAL CONTEXT AND ANNOTATION':'4. 概念语境与注释','5. CONTRIBUTOR CONFIDENCE':'5. 贡献者信心','6. VERIFICATION CHECKLIST':'6. 核查清单','1. SOURCE CHECK AND MISSING SOURCE METADATA':'1. 来源核查与缺失的来源元数据','2. DEFINITION PROVENANCE':'2. 定义来源脉络','3. CONCEPTUAL ANNOTATION':'3. 概念注释'},
  definitionType:{exact_source_wording:'与来源完全一致的原文',source_provided_translation:'来源本身提供的译文',contributor_translation_of_source:'贡献者对来源定义的翻译'},
  sourceLocationType:{'':'请选择...',page:'页码',section:'章节标题',chapter:'章',slide:'幻灯片编号',glossary_entry:'术语表词条',dictionary_entry:'词典词条',appendix:'附录',paragraph:'段落编号',table_or_figure:'表格或图',other:'其他定位方式',entry:'词典 / 术语表词条'},
  sourceType:{'':'跳过',journal_article:'期刊论文',book:'书籍',book_chapter:'书籍章节',textbook:'教材',dictionary:'词典',glossary:'术语表',website:'网站',report:'报告',policy_document:'政策文件',conference_paper:'会议论文',slides:'讲座 / 工作坊幻灯片',teaching_material:'教学材料',other:'其他'},
  sourceAccessibility:{'':'请选择...',open_access:'开放获取',institutional_access:'需要机构权限',physical_copy:'仅有纸质版本',personal_copy:'个人副本',unknown:'未知'},
  provenance:{'':'请选择...',original_to_current_source:'当前来源似乎自行提出这个定义',direct_quote_from_cited_source:'当前来源直接引用了早期被引用来源',adapted_from_cited_source:'当前来源改编或转述了早期被引用来源',synthesises_multiple_cited_sources:'当前来源综合或概述了多个早期被引用来源',provenance_unclear:'不清楚 / 未明确说明来源脉络'},
  style:{'':'不确定 / 跳过',theoretical:'理论型——解释概念的含义',operational:'操作型——说明如何测量或识别概念',normative:'规范型——说明应如何做或重视什么',descriptive:'描述型——描述常见用法或特征',procedural:'程序型——描述步骤、做法或过程',educational:'教学型——用于教学或学习说明',policy_oriented:'政策型——用于治理、规章或机构实践',other:'其他',unclear:'不清楚'},
  scope:{'':'不确定 / 跳过',very_broad:'非常宽泛',broad:'宽泛',moderate:'中等',narrow:'较窄',very_narrow:'非常狭窄'}, confidence:{'':'请选择...',5:'5 — 非常有信心',4:'4 — 有信心',3:'3 — 中等信心',2:'2 — 有些不确定',1:'1 — 不确定'}, status:{'':'不建议更改状态',source_verified:'来源已核查：定义与引用来源相符',flagged:'标记以供复核',archived:'归档 / 重复 / 已被取代'}, sourceCheck:{'':'请选择...',definition_matches_source:'已核查——定义与引用来源相符',definition_partly_matches_source:'已核查——部分相符 / 需要注意',definition_does_not_match_source:'已核查——与来源不符',source_not_accessible:'我无法获取该来源',not_checked:'我没有核查来源'},
  disciplines:{psychology:'心理学',medicine:'医学',education:'教育学',linguistics:'语言学',neuroscience:'神经科学',philosophy:'哲学',statistics:'统计学',computer_science:'计算机科学',engineering:'工程学',library_information_science:'图书馆与信息科学',sociology:'社会学',political_science:'政治学',economics_business:'经济学 / 商学',communication_media:'传播 / 媒体研究',law:'法学',humanities:'人文学科',environmental_science:'环境科学',social_science:'社会科学',interdisciplinary:'跨学科',other:'其他'},
  contexts:{open_science:'开放科学',research_methods:'研究方法',statistics:'统计学',measurement:'测量',theory_development:'理论发展',research_design:'研究设计',meta_research:'元研究',education:'教育',scientific_communication:'科学传播',research_ethics:'研究伦理',policy:'政策',clinical_practice:'临床实践',industry:'产业',student_learning:'学生学习',grant_writing:'基金申请',public_communication:'公众传播',academic_research:'学术研究',other:'其他'},
  tags:{'discipline-specific':'学科特定',discipline_specific:'学科特定',contested:'有争议','emerging-concept':'新兴概念',emerging_concept:'新兴概念',historical:'历史性',outdated:'过时',ambiguous:'含糊','jargon-heavy':'术语密集',jargon_heavy:'术语密集','accessible-to-non-experts':'非专家也容易理解',accessible_to_non_experts:'非专家也容易理解','overly-broad':'过于宽泛',overly_broad:'过于宽泛','overly-narrow':'过于狭窄',overly_narrow:'过于狭窄','frequently-used':'常用',frequently_used:'常用','rarely-used':'少用',rarely_used:'少用',normative:'规范性','value-laden':'带有价值判断',value_laden:'带有价值判断',possible_jingle_fallacy_same_label_different_meaning:'可能存在 jingle fallacy：同一标签，不同含义',possible_jangle_fallacy_different_label_similar_meaning:'可能存在 jangle fallacy：不同标签，相似含义',term_used_differently_from_other_definitions:'该术语的用法不同于其他定义',different_concept_under_same_term:'可能指向另一个概念'},
  suitable:{academic_research:'学术研究',teaching:'教学',student_learning:'学生学习',grant_writing:'基金申请',public_communication:'公众传播',policy:'政策',clinical_practice:'临床实践',industry:'产业',meta_research:'元研究'},
  checkRows:['我已按照来源中的原文准确复制定义；如有翻译，我已明确标明。','我保留了定义中出现的文内引用（如有）。','我确认这不是我自己的综合、解释或改写。','我已尽可能核查引用元数据的准确性。','我提供了页码、章节、幻灯片、词条或其他定位信息，以便他人核查定义。','我理解该提交可能会由项目贡献者进行审核、来源核查、标记或归档。'],
  statusHelp:'<strong>注意：</strong>来源已核查只表示文字和元数据看起来与引用来源相符，并不表示这是被推荐、较佳或概念上更优越的定义。'
};
FULL_FORM_I18N['zh-hant'] = {
  ...FULL_FORM_I18N['zh-hans'],
  langOptions:{en:'英語',de:'德語','zh-hans':'中文簡體','zh-hant':'中文繁體',other:'其他'}, select:'請選擇...', skip:'跳過', unsure:'不確定 / 跳過', noStatus:'不建議更改狀態', check:'檢查', submitDef:'提交定義以供審核', submitAnno:'提交註釋', remove:'移除',
  labels:{...FULL_FORM_I18N['zh-hans'].labels,'Contribute a definition':'提交定義','Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.':'請提交來自可核查來源的定義。請保留來源中的原文措辭，並保留定義內部出現的引用。','Term being defined':'被定義的術語','Term label or synonym used by the source':'來源中使用的術語標籤或同義詞','Language of submitted definition':'提交定義的語言','Definition wording type':'定義文本類型','Definition text':'定義文本','Where can another person find this definition?':'其他人在哪裡可以找到這個定義？','Locator type':'定位類型','Exact page / section / slide / entry':'具體頁碼 / 章節 / 投影片 / 詞條','Source type':'來源類型','Full citation':'完整引用','Author(s) of the current source':'當前來源的作者','Year':'年份','Title of current source':'當前來源標題','Publication outlet / publisher / organisation':'期刊 / 出版社 / 機構','Access date':'存取日期','Source accessibility':'來源可取得性','How does the current source present this definition?':'當前來源如何呈現這個定義？','Discipline(s)':'學科','Research context(s)':'研究語境','Definition style':'定義類型 / 風格','Scope of definition':'定義範圍','Why might someone choose this definition?':'為什麼有人會選用這個定義？','Community tags':'社群標籤','Suitable context(s)':'適用語境','Confidence that the definition was copied/translated accurately':'對定義複製/翻譯準確性的信心','Confidence that the source metadata are correct':'對來源元資料準確性的信心','Definition being annotated':'正在註釋的定義','Source verification':'來源核查','Source term label / synonym, if different':'來源使用的術語標籤 / 同義詞（如不同）','Full citation or correction':'完整引用或修正','How does the source present this definition?':'該來源如何呈現這個定義？','Why might someone choose, avoid, or contextualise this definition?':'為什麼有人會選用、避免或需要說明這個定義？','Annotation confidence':'註釋信心','Suggested source-check status':'建議的來源核查狀態','Tags':'標籤'},
  sections:{'1. DEFINITION INFORMATION':'1. 定義資訊','2. SOURCE INFORMATION':'2. 來源資訊','3. DEFINITION PROVENANCE':'3. 定義來源脈絡','4. CONCEPTUAL CONTEXT AND ANNOTATION':'4. 概念語境與註釋','5. CONTRIBUTOR CONFIDENCE':'5. 貢獻者信心','6. VERIFICATION CHECKLIST':'6. 核查清單','1. SOURCE CHECK AND MISSING SOURCE METADATA':'1. 來源核查與缺失的來源元資料','2. DEFINITION PROVENANCE':'2. 定義來源脈絡','3. CONCEPTUAL ANNOTATION':'3. 概念註釋'},
  definitionType:{exact_source_wording:'與來源完全一致的原文',source_provided_translation:'來源本身提供的譯文',contributor_translation_of_source:'貢獻者對來源定義的翻譯'},
  sourceLocationType:{'':'請選擇...',page:'頁碼',section:'章節標題',chapter:'章',slide:'投影片編號',glossary_entry:'術語表詞條',dictionary_entry:'詞典詞條',appendix:'附錄',paragraph:'段落編號',table_or_figure:'表格或圖',other:'其他定位方式',entry:'詞典 / 術語表詞條'},
  sourceType:{'':'跳過',journal_article:'期刊論文',book:'書籍',book_chapter:'書籍章節',textbook:'教材',dictionary:'詞典',glossary:'術語表',website:'網站',report:'報告',policy_document:'政策文件',conference_paper:'會議論文',slides:'講座 / 工作坊投影片',teaching_material:'教學材料',other:'其他'},
  sourceAccessibility:{'':'請選擇...',open_access:'開放取用',institutional_access:'需要機構權限',physical_copy:'僅有紙本版本',personal_copy:'個人副本',unknown:'未知'},
  provenance:{'':'請選擇...',original_to_current_source:'當前來源似乎自行提出這個定義',direct_quote_from_cited_source:'當前來源直接引用了早期被引用來源',adapted_from_cited_source:'當前來源改編或轉述了早期被引用來源',synthesises_multiple_cited_sources:'當前來源綜合或概述了多個早期被引用來源',provenance_unclear:'不清楚 / 未明確說明來源脈絡'},
  style:{'':'不確定 / 跳過',theoretical:'理論型——解釋概念的含義',operational:'操作型——說明如何測量或識別概念',normative:'規範型——說明應如何做或重視什麼',descriptive:'描述型——描述常見用法或特徵',procedural:'程序型——描述步驟、做法或過程',educational:'教學型——用於教學或學習說明',policy_oriented:'政策型——用於治理、規章或機構實踐',other:'其他',unclear:'不清楚'},
  scope:{'':'不確定 / 跳過',very_broad:'非常寬泛',broad:'寬泛',moderate:'中等',narrow:'較窄',very_narrow:'非常狹窄'}, confidence:{'':'請選擇...',5:'5 — 非常有信心',4:'4 — 有信心',3:'3 — 中等信心',2:'2 — 有些不確定',1:'1 — 不確定'}, status:{'':'不建議更改狀態',source_verified:'來源已核查：定義與引用來源相符',flagged:'標記以供複核',archived:'歸檔 / 重複 / 已被取代'}, sourceCheck:{'':'請選擇...',definition_matches_source:'已核查——定義與引用來源相符',definition_partly_matches_source:'已核查——部分相符 / 需要注意',definition_does_not_match_source:'已核查——與來源不符',source_not_accessible:'我無法取得該來源',not_checked:'我沒有核查來源'},
  disciplines:{psychology:'心理學',medicine:'醫學',education:'教育學',linguistics:'語言學',neuroscience:'神經科學',philosophy:'哲學',statistics:'統計學',computer_science:'電腦科學',engineering:'工程學',library_information_science:'圖書館與資訊科學',sociology:'社會學',political_science:'政治學',economics_business:'經濟學 / 商學',communication_media:'傳播 / 媒體研究',law:'法學',humanities:'人文學科',environmental_science:'環境科學',social_science:'社會科學',interdisciplinary:'跨學科',other:'其他'},
  contexts:{open_science:'開放科學',research_methods:'研究方法',statistics:'統計學',measurement:'測量',theory_development:'理論發展',research_design:'研究設計',meta_research:'元研究',education:'教育',scientific_communication:'科學傳播',research_ethics:'研究倫理',policy:'政策',clinical_practice:'臨床實踐',industry:'產業',student_learning:'學生學習',grant_writing:'基金申請',public_communication:'公眾傳播',academic_research:'學術研究',other:'其他'},
  tags:{'discipline-specific':'學科特定',discipline_specific:'學科特定',contested:'有爭議','emerging-concept':'新興概念',emerging_concept:'新興概念',historical:'歷史性',outdated:'過時',ambiguous:'含糊','jargon-heavy':'術語密集',jargon_heavy:'術語密集','accessible-to-non-experts':'非專家也容易理解',accessible_to_non_experts:'非專家也容易理解','overly-broad':'過於寬泛',overly_broad:'過於寬泛','overly-narrow':'過於狹窄',overly_narrow:'過於狹窄','frequently-used':'常用',frequently_used:'常用','rarely-used':'少用',rarely_used:'少用',normative:'規範性','value-laden':'帶有價值判斷',value_laden:'帶有價值判斷',possible_jingle_fallacy_same_label_different_meaning:'可能存在 jingle fallacy：同一標籤，不同含義',possible_jangle_fallacy_different_label_similar_meaning:'可能存在 jangle fallacy：不同標籤，相似含義',term_used_differently_from_other_definitions:'該術語的用法不同於其他定義',different_concept_under_same_term:'可能指向另一個概念'},
  suitable:{academic_research:'學術研究',teaching:'教學',student_learning:'學生學習',grant_writing:'基金申請',public_communication:'公眾傳播',policy:'政策',clinical_practice:'臨床實踐',industry:'產業',meta_research:'元研究'},
  checkRows:['我已按照來源中的原文準確複製定義；如有翻譯，我已明確標明。','我保留了定義中出現的文內引用（如有）。','我確認這不是我自己的綜合、解釋或改寫。','我已盡可能核查引用元資料的準確性。','我提供了頁碼、章節、投影片、詞條或其他定位資訊，以便他人核查定義。','我理解該提交可能會由專案貢獻者進行審核、來源核查、標記或歸檔。'],
  statusHelp:'<strong>注意：</strong>來源已核查只表示文字和元資料看起來與引用來源相符，並不表示這是被推薦、較佳或概念上更優越的定義。'
};
FULL_FORM_I18N.zh = FULL_FORM_I18N['zh-hans'];
function fullLangKey(lang){ const x=String(lang||'en').toLowerCase(); if(x.includes('hant')||x.includes('trad')) return 'zh-hant'; if(x.startsWith('zh')) return 'zh-hans'; if(x.startsWith('de')) return 'de'; return 'en'; }
function replaceTextAfterInput(label, newText){ const input=label.querySelector('input'); if(!input) return; Array.from(label.childNodes).forEach(n=>{ if(n.nodeType===3) n.nodeValue=''; }); label.appendChild(document.createTextNode(' '+newText)); }
function setSelectOptions(id,map){ const sel=document.getElementById(id); if(!sel) return; Array.from(sel.options).forEach(o=>{ const key=o.value; if(Object.prototype.hasOwnProperty.call(map,key)) o.textContent=map[key]; }); }
function setSelectsByClass(cls,map){ document.querySelectorAll('select.'+cls).forEach(sel=>Array.from(sel.options).forEach(o=>{ if(Object.prototype.hasOwnProperty.call(map,o.value)) o.textContent=map[o.value]; })); }
function localizeTagContainer(id,map){ const el=document.getElementById(id); if(!el) return; el.querySelectorAll('label').forEach(label=>{ const inp=label.querySelector('input'); if(inp && map[inp.value]) replaceTextAfterInput(label,map[inp.value]); }); }
function localizeStaticLabels(lang){ const t=FULL_FORM_I18N[fullLangKey(lang)]||FULL_FORM_I18N.en; document.querySelectorAll('.form-label').forEach(l=>{ const raw=l.textContent.replace('*','').trim().replace(/\s+/g,' '); if(t.labels[raw]) l.innerHTML=t.labels[raw]+(l.textContent.includes('*')?' <span style="color:#c0392b">*</span>':''); }); document.querySelectorAll('.form-section-title').forEach(s=>{ const raw=s.textContent.trim().toUpperCase().replace(/\s+/g,' '); if(t.sections[raw]) s.textContent=t.sections[raw]; }); }
function localizeAllFormOptions(lang){ const t=FULL_FORM_I18N[fullLangKey(lang)]||FULL_FORM_I18N.en; setSelectOptions('contribLang',t.langOptions); setSelectOptions('definitionType',t.definitionType); setSelectOptions('sourceLocationType',t.sourceLocationType); setSelectOptions('contribSourceType',t.sourceType); setSelectOptions('sourceAccessibility',t.sourceAccessibility); setSelectOptions('definitionProvenance',t.provenance); setSelectOptions('definitionStyle',t.style); setSelectOptions('definitionScope',t.scope); setSelectOptions('selfConfidence',t.confidence); setSelectOptions('metadataConfidence',t.confidence); setSelectOptions('annoSourceCheck',t.sourceCheck); setSelectOptions('annoSourceType',t.sourceType); setSelectOptions('annoSourceLocationType',t.sourceLocationType); setSelectOptions('annoDefinitionProvenance',t.provenance); setSelectOptions('annoDefinitionStyle',t.style); setSelectOptions('annoDefinitionScope',t.scope); setSelectOptions('annoConfidence',t.confidence); setSelectOptions('annoSuggestedStatus',t.status); setSelectsByClass('orig-relation',{cited_as_definition_source:t.provenance.direct_quote_from_cited_source,directly_quoted:t.provenance.direct_quote_from_cited_source,adapted_or_paraphrased:t.provenance.adapted_from_cited_source,one_of_multiple_sources:t.provenance.synthesises_multiple_cited_sources,unclear:t.provenance.provenance_unclear}); localizeTagContainer('disciplineTags',t.disciplines); localizeTagContainer('annoDisciplineTags',t.disciplines); localizeTagContainer('researchContextTags',t.contexts); localizeTagContainer('annoResearchContextTags',t.contexts); localizeTagContainer('selfTags',t.tags); localizeTagContainer('annoTags',t.tags); localizeTagContainer('suitableContextTags',t.suitable); const statusHelp=document.getElementById('annoStatusHelp'); if(statusHelp) statusHelp.innerHTML=t.statusHelp; const btn=document.getElementById('btnSubmitContrib'); if(btn) btn.textContent=t.submitDef; const abtn=document.getElementById('btnSubmitAnno'); if(abtn) abtn.textContent=t.submitAnno; document.querySelectorAll('.check-row').forEach((row,i)=>{ if(row.closest('#contribModeSubmit') && t.checkRows[i]) replaceTextAfterInput(row,t.checkRows[i]); }); document.querySelectorAll('#btnVerifyDoi').forEach(b=>b.textContent=t.check); }
function fullLocalizeSubmissionForm(){ const lang=(document.getElementById('contribLang')||{}).value||'en'; localizeStaticLabels(lang); localizeAllFormOptions(lang); }
const _oldUpdateContribLanguageHelpers = window.updateContribLanguageHelpers;
window.updateContribLanguageHelpers = function(){ if(typeof _oldUpdateContribLanguageHelpers==='function') _oldUpdateContribLanguageHelpers(); fullLocalizeSubmissionForm(); };
const _oldLocalizeAnnotationForm = window.localizeAnnotationForm;
window.localizeAnnotationForm = function(lang){ if(typeof _oldLocalizeAnnotationForm==='function') _oldLocalizeAnnotationForm(lang); localizeStaticLabels(lang); localizeAllFormOptions(lang); };
document.addEventListener('DOMContentLoaded',()=>{ const langSel=document.getElementById('contribLang'); if(langSel){ langSel.addEventListener('change',fullLocalizeSubmissionForm); } });

// ── Supabase config ──────────────────────────────────────────
// These values are injected at deploy time by GitHub Actions.
// See env-config.js which is generated by the deploy workflow.
const SUPABASE_URL     = window.__ENV__?.SUPABASE_URL     || '';
const SUPABASE_ANON_KEY = window.__ENV__?.SUPABASE_ANON_KEY || '';
const SHINY_URL        = 'https://msleungyi.shinyapps.io/re-searchterms-forrt_v2/';

let supa = null;
try {
  if (!SUPABASE_URL || !SUPABASE_ANON_KEY) {
    console.error('[Re-SearchTerms] Supabase env vars not set. Ensure env-config.js is generated by the deploy workflow.');
  } else {
    supa = supabase.createClient(SUPABASE_URL, SUPABASE_ANON_KEY);
    console.info('[Re-SearchTerms] Supabase client ready:', SUPABASE_URL);
  }
} catch(e) { console.error('[Re-SearchTerms] Supabase init error:', e); }

let allTerms = [], currentUser = null, pendingContributeTerm = null;
let latestPanelDefinitions = [];

// ── Init ──────────────────────────────────────────────────────
(async () => {
  try {
    await handleAuthCallback();
    await checkAuth();
    await loadTerms();
    loadStats();
    handleUrlParams();
  } catch(e) { console.error('Init error:', e); }
})();

// ── Auth ──────────────────────────────────────────────────────
async function checkAuth() {
  if (!supa) return;
  const { data: { session } } = await supa.auth.getSession();
  if (session) { currentUser = session.user; renderAuthArea(); }
  supa.auth.onAuthStateChange((event, session) => {
    currentUser = session?.user || null;
    renderAuthArea();
    if (event === 'SIGNED_IN') closeModal();
  });
}

function renderAuthArea() {
  const area = document.getElementById('authArea');
  if (currentUser) {
    const initial = (currentUser.user_metadata?.full_name || currentUser.email || '?')[0].toUpperCase();
    const name = currentUser.user_metadata?.full_name || currentUser.email;
    area.innerHTML = `<div class="user-chip"><div class="user-avatar">${initial}</div><span>${name}</span><button class="btn-signout" onclick="doSignout()">Sign out</button></div>`;
  } else {
    area.innerHTML = `<button class="btn-login" onclick="openModal('login')">Log in</button><button class="btn-signup" onclick="openTallyModal()">Sign up to contribute</button>`;
  }
}

async function doLogin() {
  const email = document.getElementById('loginEmail').value.trim();
  const password = document.getElementById('loginPassword').value;
  const msg = document.getElementById('loginMsg');
  const btn = document.getElementById('btnLogin');
  if (!email || !password) { showMsg(msg, 'Please fill in all fields.', 'error'); return; }
  btn.disabled = true; btn.textContent = 'Logging in…';
  const { error } = await supa.auth.signInWithPassword({ email, password });
  btn.disabled = false; btn.textContent = 'Log in';
  if (error) { showMsg(msg, error.message, 'error'); }
  else { closeModal(); }
}

async function doSignout() { await supa.auth.signOut(); }

// ── Tally Modal ───────────────────────────────────────────────
function openTallyModal() {
  const bd = document.getElementById('tallyBackdrop');
  bd.style.display = 'flex';
  document.body.style.overflow = 'hidden';
}
function closeTallyModal() {
  document.getElementById('tallyBackdrop').style.display = 'none';
  document.body.style.overflow = '';
}

// ── Terms ─────────────────────────────────────────────────────
async function loadTerms() {
  if (!supa) {
    document.getElementById('termGrid').innerHTML =
      '<div class="empty">Supabase client not initialised — check that env-config.js is loaded and contains valid credentials.</div>';
    return;
  }
  const { data, error } = await supa.from('v_term_coverage').select('*').order('name_en');
  if (error || !data) { document.getElementById('termGrid').innerHTML = '<div class="empty">Could not load terms. Please try again later.</div>'; return; }
  allTerms = data;
  renderTerms(data);
}

function loadStats() {
  if (!allTerms.length) return;
  const totalDefs = allTerms.reduce((s, t) => s + (t.def_count_total || 0), 0);
  const set = (id, val) => { const el = document.getElementById(id); if (el) el.textContent = val; };
  set('statTerms', allTerms.length);
  set('statDefs', totalDefs);
  set('statEN', allTerms.filter(t => t.def_count_en > 0).length);
  set('statDE', allTerms.filter(t => t.def_count_de > 0).length);
  set('statZH', allTerms.filter(t => t.def_count_zh > 0).length);
}

function renderTerms(terms) {
  var grid = document.getElementById('termGrid');
  document.getElementById('resultsCount').textContent = terms.length + ' terms';
  if (!terms.length) {
    const q = (document.getElementById('searchInput')?.value || '').trim();
    grid.innerHTML = '<div class="empty">No terms match your search.<br><button class="new-term-btn" style="margin-top:1rem" onclick="openNewTermContrib(event, ' + JSON.stringify(q).replace(/"/g,'&quot;') + ')">+ Suggest this as a new term</button></div>';
    return;
  }
  grid.innerHTML = terms.map(function(t) {
    var id = t.id;
    var nameEn = escHtml(t.name_en);
    var slug = escHtml(t.slug);
    var total = t.def_count_total || 0;
    var pending = t.pending_count || 0;
    return '<div class="term-card" onclick="openPanel(&#39;' + id + '&#39;)">'
      + '<div class="term-card-top">'
      + '<div>'
      + '<div class="term-name">' + nameEn + '</div>'
      + renderTermTranslations(t)
      + '</div>'
      + '<span class="term-status-badge badge-established">Established</span>'
      + '</div>'
      + '<div class="lang-pills">'
      + langPill('EN', t.def_count_en, t.name_en)
      + langPill('DE', t.def_count_de, t.name_de)
      + langPill('ZH', t.def_count_zh, t.name_zh)
      + '</div>'
      + '<div class="def-count"><strong>' + total + '</strong> approved definition' + (total !== 1 ? 's' : '')
      + (pending > 0 ? ' &middot; <span style="color:var(--accent)">' + pending + ' pending</span>' : '')
      + '</div>'
      + '<div class="term-card-actions">'
      + '<button class="card-btn card-btn-analyse" onclick="goToDefinitionTerm(&#39;' + slug + '&#39;, event)">Definition graph &#8599;</button>'
      + '<button class="card-btn card-btn-analyse" onclick="goToWordTerm(&#39;' + slug + '&#39;, event)">Word-level &#8599;</button>'
      + '<button class="card-btn card-btn-contribute" onclick="openContrib(event,&#39;' + id + '&#39;,&#39;' + nameEn + '&#39;)">+ Contribute</button>'
      + '</div>'
      + '</div>';
  }).join('');
}

function renderTermTranslations(t) {
  const parts = [];
  if (t.name_de) parts.push('<span class="term-translation"><strong>DE</strong>' + escHtml(t.name_de) + '</span>');
  if (t.name_zh) parts.push('<span class="term-translation"><strong>ZH</strong>' + escHtml(t.name_zh) + '</span>');
  if (t.name_zh_hant && t.name_zh_hant !== t.name_zh) parts.push('<span class="term-translation"><strong>ZH-TW</strong>' + escHtml(t.name_zh_hant) + '</span>');
  return parts.length ? '<div class="term-translations">' + parts.join('') + '</div>' : '';
}

function langPill(code, count, name) {
  const cls = code==='EN'?'pill-en':code==='DE'?'pill-de':'pill-zh';
  return `<span class="lang-pill ${count>0?cls:'pill-missing'}" title="${name||''}">${code}${count>0?' ✓':' —'}</span>`;
}

function filterTerms() {
  const q = document.getElementById('searchInput').value.toLowerCase();
  const lang = document.getElementById('langFilter').value;
  const sort = document.getElementById('sortFilter').value;
  let f = allTerms.filter(t => {
    const mq = !q || (t.name_en||'').toLowerCase().includes(q) || (t.name_de||'').toLowerCase().includes(q) || (t.name_zh||'').toLowerCase().includes(q);
    const ml = lang===''?true:lang==='missing-de'?(!t.def_count_de||t.def_count_de===0):lang==='missing-zh'?(!t.def_count_zh||t.def_count_zh===0):lang==='has-community'?(t.def_count_total>t.def_count_en):true;
    return mq && ml;
  });
  f.sort((a,b) => sort==='defs-desc'?(b.def_count_total||0)-(a.def_count_total||0):sort==='defs-asc'?(a.def_count_total||0)-(b.def_count_total||0):(a.name_en||'').localeCompare(b.name_en||''));
  renderTerms(f);
}

// ── Panel ─────────────────────────────────────────────────────
async function openPanel(termId) {
  document.getElementById('panelBackdrop').classList.add('open');
  document.getElementById('panelContent').innerHTML = '<div class="loading"><div class="loading-spinner"></div>Loading…</div>';
  const term = allTerms.find(t => t.id === termId);
  if (!term) return;
  let result = await supa
    .from('v_definitions_public')
    .select('*')
    .eq('term_id', termId)
    .order('language');

  if (result.error) {
    console.warn('v_definitions_public not available; falling back to definitions table:', result.error);
    result = await supa
      .from('definitions')
      .select('*')
      .eq('term_id', termId)
      .order('language');
  }

  let defs = result.data || [];
  if (result.error) console.error('Definitions fetch error:', result.error);

  defs = (defs || []).filter(d =>
    !['rejected','archived'].includes(String(d.status || '').toLowerCase()) &&
    !['rejected','archived'].includes(String(d.validation_status || '').toLowerCase())
  );
  latestPanelDefinitions = defs;

  const byLang = { en: [], de: [], zh_hans: [], zh_hant: [] };
  defs.forEach(d => {
    const lang = String(d.language || '').toLowerCase();
    if (lang.startsWith('de')) byLang.de.push(d);
    else if (lang.includes('hant') || lang.includes('trad') || lang.includes('繁')) byLang.zh_hant.push(d);
    else if (lang.startsWith('zh') || lang.includes('chinese') || lang.includes('中文')) byLang.zh_hans.push(d);
    else byLang.en.push(d);
  });

  const nameEn = escHtml(term.name_en);
  const countAll = defs.length;
  const tabCounts = {
    all: countAll,
    en: byLang.en.length,
    de: byLang.de.length,
    zh_hans: byLang.zh_hans.length,
    zh_hant: byLang.zh_hant.length
  };

  let panelHtml = '<div class="panel-term-name" data-term-id="' + escHtml(termId) + '">' + nameEn + '</div>'
    + '<div class="panel-translations">'
    + (term.name_de ? '<span class="lang-pill pill-de">DE: ' + escHtml(term.name_de) + '</span>' : '')
    + (term.name_zh ? '<span class="lang-pill pill-zh">ZH: ' + escHtml(term.name_zh) + '</span>' : '')
    + '</div>'
    + renderSmartLinkRow(term.slug || term.name_en, 'Explore this term')
    + '<div class="panel-lang-tabs" role="tablist">'
    + panelTabButton('all', 'All', tabCounts.all, true)
    + panelTabButton('en', 'English', tabCounts.en, false)
    + panelTabButton('de', 'Deutsch', tabCounts.de, false)
    + panelTabButton('zh', '中文（简/繁）', (tabCounts.zh_hans||0)+(tabCounts.zh_hant||0), false)
    + '</div>'
    + '<div id="pane-all" class="panel-lang-pane active">'
    + renderContributionCta(termId, nameEn, 'all')
    + renderDefSection('All Definitions', defs, null)
    + '</div>'
    + '<div id="pane-en" class="panel-lang-pane">'
    + renderContributionCta(termId, nameEn, 'en')
    + renderDefSection('English Definitions', byLang.en, term.forrt_url_en)
    + '</div>'
    + '<div id="pane-de" class="panel-lang-pane">'
    + renderContributionCta(termId, nameEn, 'de')
    + renderDefSection('German Definitions', byLang.de, term.forrt_url_de)
    + '</div>'
    + '<div id="pane-zh" class="panel-lang-pane">'
    + renderContributionCta(termId, nameEn, 'zh-hans')
    + renderDefSection('Chinese Definitions (简/繁)', [...(byLang.zh_hans||[]),...(byLang.zh_hant||[])], term.forrt_url_zh)
    + '</div>';
  document.getElementById('panelContent').innerHTML = panelHtml;
}

function panelTabButton(id, label, count, active) {
  return '<button type="button" class="panel-lang-tab' + (active ? ' active' : '') + '" onclick="switchPanelLang(\'' + id + '\')">' + label + ' (' + count + ')</button>';
}

function switchPanelLang(id) {
  document.querySelectorAll('.panel-lang-tab').forEach(b => b.classList.remove('active'));
  document.querySelectorAll('.panel-lang-pane').forEach(p => p.classList.remove('active'));
  const pane = document.getElementById('pane-' + id);
  if (pane) pane.classList.add('active');
  const buttons = Array.from(document.querySelectorAll('.panel-lang-tab'));
  const btn = buttons.find(b => b.getAttribute('onclick') && b.getAttribute('onclick').includes("'" + id + "'"));
  if (btn) btn.classList.add('active');
}

function renderContributionCta(termId, nameEn, lang) {
  const buttons = lang === 'all'
    ? '<button onclick="openContrib(null,\'' + termId + '\',\'' + nameEn + '\',\'en\')">+ English source</button>'
      + '<button onclick="openContrib(null,\'' + termId + '\',\'' + nameEn + '\',\'de\')">+ German source</button>'
      + '<button onclick="openContrib(null,\'' + termId + '\',\'' + nameEn + '\',\'zh-hans\')">+ Chinese Simplified source</button>'
      + ''
    : '<button onclick="openContrib(null,\'' + termId + '\',\'' + nameEn + '\',\'' + lang + '\')">+ Add a definition in this language</button>';
  return '<div class="panel-cta">'
    + '<p><strong>Contribute a definition for this term.</strong></p>'
    + '<p style="font-size:.8rem;margin:.3rem 0 .5rem;">We welcome three types of contributions:</p>'
    + '<ul style="font-size:.8rem;margin:.2rem 0 .65rem;padding-left:1.1rem;line-height:1.75;">'
    + '<li><strong>Source-original</strong> — a definition as written by the original source (paper, book, glossary, report).</li>'
    + '<li><strong>Source-translated</strong> — a translation provided by the source itself (e.g. a bilingual publication).</li>'
    + '<li><strong>Contributor-translated</strong> — your own translation of a source definition (e.g. English to German or Chinese).</li>'
    + '</ul>'
    + '<p style="font-size:.8rem;color:var(--ink-mid);">Select the language of the definition you are submitting:</p>'
    + '<div class="panel-lang-actions">' + buttons + '</div></div>';
}

function sourceLabel(d) {
  if (d.source_type === 'glossary_seed')        return '<span class="src-badge src-forrt">FORRT Glossary</span>';
  if (d.source_type === 'publisher_dictionary') return '<span class="src-badge src-igi">IGI InfoSci-Dictionary</span>';
  if (d.source_type === 'wiktionary_api')       return '<span class="src-badge src-wikt">Wiktionary</span>';
  if (d.profiles && d.profiles.display_name) {
    var orcid = d.profiles.orcid
      ? ' <a href="https://orcid.org/' + escHtml(d.profiles.orcid) + '" target="_blank" style="color:#a6ce39;font-size:.7rem;">ORCID</a>'
      : '';
    return '<span class="src-badge src-community">&#9997; ' + escHtml(d.profiles.display_name) + orcid + '</span>';
  }
  return '';
}

function verifiedBadge(d) {
  if (!['verified','source_verified'].includes(String(d.validation_status || '').toLowerCase())) return '';
  var name = d.verified_by_name ? escHtml(d.verified_by_name) : 'community';
  return '<span class="verified-star">&#11088; Source-verified by ' + name + '</span>';
}

function renderDefSection(title, defs, forrtUrl) {
  if (!defs.length && !forrtUrl) return '';
  var html = '<div class="panel-section">';
  html += '<div class="panel-section-title">' + title + '</div>';
  if (defs.length) {
    defs.forEach(function(d) {
      var verified = ['verified','source_verified'].includes(String(d.validation_status || '').toLowerCase());
      html += '<div class="definition-card' + (verified ? ' definition-card--verified' : '') + '" data-def-id="' + d.id + '" data-def-lang="' + escHtml(d.language || '') + '" data-def-source="' + escHtml(d.source_type || '') + '" data-def-author="' + escHtml(d.citation_author || '') + '" data-def-year="' + escHtml(String(d.citation_year || '')) + '" data-def-location="' + escHtml(d.source_location || '') + '">';
      html += verifiedBadge(d);
      html += '<div class="definition-text">' + escHtml(d.definition_text);
      if (d.is_truncated) html += '<span style="color:var(--ink-lt)"> [truncated]</span>';
      html += '</div>';
      html += '<div class="definition-meta">' + sourceLabel(d);
      if (d.citation_author) html += '<span>' + escHtml(d.citation_author) + (d.citation_year ? ', ' + d.citation_year : '') + '</span>';
      if (d.source_term_label) html += '<span>Source label: ' + escHtml(d.source_term_label) + '</span>';
      if (d.citation_doi) html += '<a href="https://doi.org/' + escHtml(d.citation_doi) + '" target="_blank">DOI &#8599;</a>';
      if (d.citation_url) html += '<a href="' + escHtml(d.citation_url) + '" target="_blank" style="color:var(--green-mid);font-weight:600;">Source &#8599;</a>';
      if (d.source_location) html += '<span>Locator: ' + escHtml(d.source_location) + '</span>';
      html += '</div>';
      if (currentUser) {
        html += '<div style="margin-top:.5rem;display:flex;gap:.4rem;flex-wrap:wrap;">';
        html += '<button class="card-btn card-btn-contribute" style="font-size:.7rem;padding:.2rem .6rem;" data-def-id="' + d.id + '" onclick="handleAnnotate(this)">+ Annotate / source-check</button>';
        html += '</div>';
      }
      html += '</div>';
    });
  } else {
    html += '<div style="font-size:.85rem;color:var(--ink-lt);padding:.5rem 0">No stored definitions are currently returned for this language. If the term card shows a definition count, the count is probably coming from a coverage table or seed data that has not yet been exposed through <code>v_definitions_public</code> or <code>definitions</code>.</div>';
  }
  html += '</div>';
  return html;
}

// Use data attributes instead of inline params to avoid quote issues
function handleAnnotate(btn) {
  var defId = btn.getAttribute('data-def-id');
  openAnnotate(null, defId);
}

function handleVerify(btn) {
  var defId = btn.getAttribute('data-def-id');
  verifyDefinition(null, defId, btn);
}



function closePanel(e) {
  if (e && e.target !== document.getElementById('panelBackdrop')) return;
  const bd = document.getElementById('panelBackdrop');
  if (bd) bd.classList.remove('open');
}


// Extra robust close behaviour for the right-side term panel.
document.addEventListener('click', function(e) {
  if (e.target && e.target.classList && e.target.classList.contains('panel-close')) {
    e.preventDefault();
    const bd = document.getElementById('panelBackdrop');
    if (bd) bd.classList.remove('open');
  }
});

// ── Analysis ──────────────────────────────────────────────────

function smartSlug(x) {
  return String(x || '').trim().toLowerCase()
    .replace(/&/g, ' and ')
    .replace(/[’'`]/g, '')
    .replace(/[^a-z0-9]+/g, '_')
    .replace(/^_+|_+$/g, '');
}
function termFromKey(key) {
  const raw = String(key || '').trim();
  if (!raw) return null;
  const rawSlug = smartSlug(raw);
  const fromSupabase = (window.allTerms || []).find(t =>
    String(t.slug || '').toLowerCase() === raw.toLowerCase() ||
    smartSlug(t.name_en) === rawSlug ||
    smartSlug(t.name_de) === rawSlug ||
    smartSlug(t.name_zh) === rawSlug ||
    String(t.id || '') === raw
  );
  if (fromSupabase) return { slug: fromSupabase.slug || smartSlug(fromSupabase.name_en), name: fromSupabase.name_en, id: fromSupabase.id };
  const defTerms = (legacyDefinitionNodes || []).map(d => d.concept).filter(Boolean);
  const matchDef = defTerms.find(t => smartSlug(t) === rawSlug || String(t).toLowerCase() === raw.toLowerCase());
  if (matchDef) return { slug: smartSlug(matchDef), name: matchDef, id: null };
  const wordTerms = wordLevelData ? Object.keys(wordLevelData) : [];
  const matchWord = wordTerms.find(t => smartSlug(t) === rawSlug || String(t).toLowerCase() === raw.toLowerCase());
  if (matchWord) return { slug: smartSlug(matchWord), name: matchWord, id: null };
  return { slug: rawSlug, name: raw.replace(/_/g, ' '), id: null };
}
function smartRoute(page, termKey, extra = {}) {
  const t = termFromKey(termKey) || { slug: smartSlug(termKey), name: String(termKey || '') };
  const params = new URLSearchParams({ term: t.slug, ...extra });
  history.replaceState(null, '', `#${page}?${params.toString()}`);
}
function renderSmartLinkRow(termKey, label) {
  const safe = escHtml(String(termKey || ''));
  return `<div class="smart-link-row"><span class="smart-link-label">${escHtml(label || 'Related views')}</span>` +
    `<button class="smart-link-btn primary" onclick="goToDefinitionTerm('${safe}', event)">Definition graph</button>` +
    `<button class="smart-link-btn" onclick="goToWordTerm('${safe}', event)">Word-level</button>` +
    `<button class="smart-link-btn" onclick="goToTermAnalysis('${safe}', 'cooccurrence', event)">Term-level</button></div>`;
}
async function goToDefinitionTerm(termKey, event) {
  if (event) event.stopPropagation();
  const bd = document.getElementById('panelBackdrop'); if (bd) bd.classList.remove('open');
  const t = termFromKey(termKey);
  showPage('definitions');
  await initDefinitionNetworkPage();
  const sel = document.getElementById('definitionTermSelect');
  if (sel && t?.name) sel.value = t.name;
  smartRoute('definitions', termKey);
  renderDefinitionNetwork();
}
async function goToWordTerm(termKey, event) {
  if (event) event.stopPropagation();
  const bd = document.getElementById('panelBackdrop'); if (bd) bd.classList.remove('open');
  const t = termFromKey(termKey);
  showPage('words');
  await initWordLevelPage();
  const sel = document.getElementById('wordTermSelect');
  if (sel && t?.name) sel.value = t.name;
  smartRoute('words', termKey);
  renderWordLevel();
}
async function goToTermAnalysis(termKey, mode = 'cooccurrence', event) {
  if (event) event.stopPropagation();
  const bd = document.getElementById('panelBackdrop'); if (bd) bd.classList.remove('open');
  const t = termFromKey(termKey);
  showPage('terms');
  await initTermLevelPage();
  switchTermMode(mode);
  const targets = ['termCooccurrenceSelect','termClusterSelect'];
  targets.forEach(id => { const el = document.getElementById(id); if (el && t?.name) el.value = t.name; });
  smartRoute('terms', termKey, { mode });
  if (mode === 'cluster') renderTermCluster(); else if (mode === 'typestokens') renderTypesTokens(); else renderTermCooccurrence();
}
function getCurrentDefinitionTerm() { return document.getElementById('definitionTermSelect')?.value || ''; }
function getCurrentWordTerm() { return document.getElementById('wordTermSelect')?.value || ''; }
function getCurrentTermLevelTerm() { return document.getElementById('termCooccurrenceSelect')?.value || document.getElementById('termClusterSelect')?.value || ''; }
function goFromDefinitionsToWords(e){ goToWordTerm(getCurrentDefinitionTerm(), e); }
function goFromDefinitionsToTerms(e){ goToTermAnalysis(getCurrentDefinitionTerm(), 'cooccurrence', e); }
function goFromWordToDefinitions(e){ goToDefinitionTerm(getCurrentWordTerm(), e); }
function goFromWordToTerms(e){ goToTermAnalysis(getCurrentWordTerm(), 'cooccurrence', e); }
function goFromTermsToDefinitions(e){ goToDefinitionTerm(getCurrentTermLevelTerm(), e); }
function goFromTermsToWords(e){ goToWordTerm(getCurrentTermLevelTerm(), e); }
function openAnalysis(event, slug) { goToDefinitionTerm(slug, event); }
async function handleSmartDeepLink() {
  const raw = location.hash.replace(/^#/, '');
  if (!raw) return;
  const [page, query] = raw.split('?');
  const params = new URLSearchParams(query || '');
  const term = params.get('term');
  const mode = params.get('mode') || 'cooccurrence';
  if (page === 'definitions' && term) return goToDefinitionTerm(term);
  if (page === 'words' && term) return goToWordTerm(term);
  if (page === 'terms' && term) return goToTermAnalysis(term, mode);
  if (document.getElementById(`page-${page}`)) showPage(page);
}

// ── Community Dashboard / Leaderboard ─────────────────────────
let leaderboardEvents = [];
let leaderboardRows = [];
let expandedContributorId = null;
let currentCommunityView = 'leaderboard';

function normaliseOrcid(orcid) {
  if (!orcid) return '';
  return String(orcid).trim().replace(/^https?:\/\/orcid\.org\//i, '').replace(/^orcid:\s*/i, '');
}
function contributorLabel(row) {
  return row.display_name || row.full_name || row.name || row.contributor_name || row.email || row.contributor_id || 'Unknown contributor';
}
function eventScore(e) { return e.type === 'definition' ? 3 : (e.type === 'annotation' ? 2 : 0); }
function langBucket(lang) {
  const x = String(lang || '').toLowerCase();
  if (x.startsWith('en')) return 'en';
  if (x.startsWith('de')) return 'de';
  if (x.startsWith('zh')) return 'zh';
  return 'other';
}
function langLabel(lang) {
  const b = langBucket(lang);
  return b === 'en' ? 'English' : b === 'de' ? 'German' : b === 'zh' ? 'Chinese' : 'Other';
}
function parseDateMaybe(x) { const d = x ? new Date(x) : null; return d && !isNaN(d.getTime()) ? d : null; }
function shortDate(x) { const d = parseDateMaybe(x); return d ? d.toISOString().slice(0,10) : '—'; }
function monthStart() { const n = new Date(); return new Date(n.getFullYear(), n.getMonth(), 1); }

async function loadLeaderboard() {
  const loading = document.getElementById('communityLoading');
  if (loading) loading.style.display = 'block';
  leaderboardEvents = await buildLeaderboardEventsFromBaseTables();
  if (loading) loading.style.display = 'none';
  renderCommunityDashboard();
}

async function buildLeaderboardEventsFromBaseTables() {
  const events = [];

  const termsResp = await supa.from('terms').select('id, name_en, name_de, name_zh, slug').limit(10000);
  const termById = new Map((termsResp.data || []).map(t => [t.id, t]));

  const allDefsResp = await supa
    .from('v_definitions_public')
    .select('id, term_id, contributor_id, language, created_at, definition_text, source_type, source_term_label, citation_author, citation_year, citation_title, citation_url, full_citation, validation_status, status')
    .limit(10000);

  const defById = new Map((allDefsResp.data || []).map(d => [d.id, d]));

  (allDefsResp.data || []).forEach(d => {
    if (!d.contributor_id) return;
    const term = termById.get(d.term_id) || {};
    events.push({
      type: 'definition',
      contributor_id: d.contributor_id,
      language: d.language,
      created_at: d.created_at,
      definition_id: d.id,
      term_id: d.term_id,
      term_name: term.name_en || d.source_term_label || '(term unavailable)',
      term_name_de: term.name_de || '',
      term_name_zh: term.name_zh || '',
      source_type: d.source_type,
      citation: d.full_citation || [d.citation_author, d.citation_year, d.citation_title].filter(Boolean).join(' · '),
      definition_preview: String(d.definition_text || '').slice(0, 170),
      status: d.validation_status || d.status || ''
    });
  });

  const annResp = await supa
    .from('definition_annotations')
    .select('id, contributor_id, definition_id, created_at, source_check, tags, annotation_note')
    .not('contributor_id', 'is', null)
    .limit(10000);

  (annResp.data || []).forEach(a => {
    const d = defById.get(a.definition_id) || {};
    const term = termById.get(d.term_id) || {};
    events.push({
      type: 'annotation',
      contributor_id: a.contributor_id,
      language: d.language,
      created_at: a.created_at,
      definition_id: a.definition_id,
      annotation_id: a.id,
      term_id: d.term_id,
      term_name: term.name_en || d.source_term_label || '(term unavailable)',
      term_name_de: term.name_de || '',
      term_name_zh: term.name_zh || '',
      source_type: d.source_type,
      citation: d.full_citation || [d.citation_author, d.citation_year, d.citation_title].filter(Boolean).join(' · '),
      definition_preview: String(d.definition_text || '').slice(0, 170),
      source_check: a.source_check || '',
      tags: a.tags || [],
      annotation_note: a.annotation_note || ''
    });
  });

  // Profile enrichment: supports profiles or v_contributor_profiles if available.
  const userIds = Array.from(new Set(events.map(e => e.contributor_id).filter(Boolean)));
  let profiles = [];
  if (userIds.length) {
    try {
      const pResp = await supa.from('v_contributor_profiles').select('*').in('id', userIds);
      if (!pResp.error && pResp.data) profiles = pResp.data;
    } catch(e) {}
    if (!profiles.length) {
      try {
        const pResp2 = await supa.from('profiles').select('*').in('id', userIds);
        if (!pResp2.error && pResp2.data) profiles = pResp2.data;
      } catch(e) {}
    }
  }
  const profileById = new Map(profiles.map(p => [p.id || p.user_id || p.contributor_id, p]));
  events.forEach(e => {
    const p = profileById.get(e.contributor_id);
    if (p) {
      e.display_name = p.display_name || p.full_name || p.name || p.email;
      e.orcid = p.orcid || p.orcid_id || p.orcid_url || '';
      e.affiliation = p.affiliation || p.institution || '';
    }
  });
  return events;
}

function filteredLeaderboardEvents() {
  const range = document.getElementById('leaderboardTimeRange')?.value || 'month';
  const lang = document.getElementById('leaderboardLanguage')?.value || 'all';
  const now = new Date();
  return leaderboardEvents.filter(e => {
    if (lang !== 'all' && langBucket(e.language) !== lang) return false;
    const dt = parseDateMaybe(e.created_at);
    if (range === 'all') return true;
    if (!dt) return false;
    let cutoff;
    if (range === 'month') cutoff = monthStart();
    else cutoff = new Date(now.getTime() - Number(range) * 24 * 60 * 60 * 1000);
    return dt >= cutoff;
  });
}

function buildRowsFromEvents(events) {
  const byUser = new Map();
  function ensure(uid) {
    if (!byUser.has(uid)) byUser.set(uid, {
      contributor_id: uid, display_name: uid, orcid: '', affiliation: '',
      definitions: 0, annotations: 0, en: 0, de: 0, zh: 0, other_lang: 0,
      terms: new Set(), languages: new Set(), score: 0, first_activity: null, last_activity: null, activities: []
    });
    return byUser.get(uid);
  }
  events.forEach(e => {
    if (!e.contributor_id) return;
    const row = ensure(e.contributor_id);
    if (e.display_name) row.display_name = e.display_name;
    if (e.orcid) row.orcid = e.orcid;
    if (e.affiliation) row.affiliation = e.affiliation;
    if (e.type === 'definition') row.definitions += 1;
    if (e.type === 'annotation') row.annotations += 1;
    const lb = langBucket(e.language);
    if (lb === 'en') row.en += 1; else if (lb === 'de') row.de += 1; else if (lb === 'zh') row.zh += 1; else row.other_lang += 1;
    row.languages.add(lb);
    if (e.term_id || e.term_name) row.terms.add(e.term_id || e.term_name);
    row.score += eventScore(e);
    row.activities.push(e);
    const dt = parseDateMaybe(e.created_at);
    if (dt && (!row.last_activity || dt > row.last_activity)) row.last_activity = dt;
    if (dt && (!row.first_activity || dt < row.first_activity)) row.first_activity = dt;
  });
  return Array.from(byUser.values()).map(r => ({
    ...r,
    _name: contributorLabel(r),
    _orcid: normaliseOrcid(r.orcid),
    _definitions: r.definitions,
    _annotations: r.annotations,
    _approved_en: r.en,
    _approved_de: r.de,
    _approved_zh: r.zh,
    _language_count: r.languages.size,
    _term_count: r.terms.size,
    _score: r.score,
    _first_activity: r.first_activity,
    _last_activity: r.last_activity,
    _activities: (r.activities || []).slice().sort((a,b)=>(parseDateMaybe(b.created_at)?.getTime()||0)-(parseDateMaybe(a.created_at)?.getTime()||0))
  }));
}

function getFilteredRows() {
  const q = (document.getElementById('leaderboardSearch')?.value || '').toLowerCase().trim();
  const sort = document.getElementById('leaderboardSort')?.value || 'score';
  let rows = buildRowsFromEvents(filteredLeaderboardEvents()).filter(r => {
    if (!q) return true;
    const hay = [r._name, r.affiliation, r.contributor_id, r._orcid, ...(r._activities||[]).map(a => a.term_name)].filter(Boolean).join(' ').toLowerCase();
    return hay.includes(q);
  });
  rows.sort((a,b) => {
    if (sort === 'name') return String(a._name).localeCompare(String(b._name));
    if (sort === 'definitions') return b._definitions - a._definitions;
    if (sort === 'annotations') return b._annotations - a._annotations;
    if (sort === 'languages') return b._language_count - a._language_count;
    if (sort === 'terms') return b._term_count - a._term_count;
    if (sort === 'recent') return (b._last_activity?.getTime() || 0) - (a._last_activity?.getTime() || 0);
    return b._score - a._score;
  });
  leaderboardRows = rows;
  return rows;
}

function updateLeaderboardSummary(rows) {
  const summary = document.getElementById('leaderboardSummary');
  if (!summary) return;
  const contributors = rows.length;
  const defs = rows.reduce((s,r)=>s+r._definitions,0);
  const ann = rows.reduce((s,r)=>s+r._annotations,0);
  const langSet = new Set();
  rows.forEach(r => { if (r._approved_en) langSet.add('en'); if (r._approved_de) langSet.add('de'); if (r._approved_zh) langSet.add('zh'); if (r.other_lang) langSet.add('other'); });
  summary.innerHTML = `
    <div class="leaderboard-stat"><div class="leaderboard-stat-num">${contributors}</div><div class="leaderboard-stat-label">Contributors</div></div>
    <div class="leaderboard-stat"><div class="leaderboard-stat-num">${defs}</div><div class="leaderboard-stat-label">Definitions submitted</div></div>
    <div class="leaderboard-stat"><div class="leaderboard-stat-num">${ann}</div><div class="leaderboard-stat-label">Annotations</div></div>
    <div class="leaderboard-stat"><div class="leaderboard-stat-num">${langSet.size}</div><div class="leaderboard-stat-label">Languages covered</div></div>`;
}

function contributorBadges(row) {
  const badges = [];
  if (row._definitions >= 50) badges.push('🏛️ Senior Curator'); else if (row._definitions >= 10) badges.push('📚 Definition Curator');
  if (row._annotations >= 100) badges.push('🧠 Concept Reviewer'); else if (row._annotations >= 25) badges.push('🔍 Annotation Specialist');
  if (row._language_count >= 4) badges.push('🗺️ Polyglot Contributor'); else if (row._language_count >= 3) badges.push('🌏 Trilingual Contributor'); else if (row._language_count >= 2) badges.push('🌍 Bilingual Contributor');
  if (row._term_count >= 25) badges.push('🧩 Concept Mapper');
  if ((row._activities || []).length > 0) badges.push('🌱 Contributor');
  return badges;
}

function openContributionInDefinitionNetwork(definitionId, termName, language) {
  if (!definitionId || !termName) return;
  showPage('definitions');
  setTimeout(async () => {
    await initDefinitionNetworkPage();
    const termSel = document.getElementById('definitionTermSelect');
    const langSel = document.getElementById('definitionLangSelect');
    if (termSel) termSel.value = termName;
    if (langSel) langSel.value = langBucket(language) === 'zh' ? 'zh' : (langBucket(language) || 'all');
    await renderDefinitionNetwork();
    const liveNodeId = 'live:' + definitionId;
    if (currentDefinitionGraphItems && currentDefinitionGraphItems.has(liveNodeId)) {
      showDefinitionNodeDetails(liveNodeId);
      try { definitionNetwork?.selectNodes([liveNodeId]); definitionNetwork?.focus(liveNodeId, { scale: 1.25, animation: true }); } catch(e) {}
    }
  }, 80);
}

function contributionItemHtml(a) {
  const typeLabel = a.type === 'definition' ? 'Submitted definition' : 'Annotation';
  const term = a.term_name || '(term unavailable)';
  const lang = langLabel(a.language);
  const date = shortDate(a.created_at);
  const citation = a.citation ? `<div class="contribution-item-sub">${escHtml(a.citation)}</div>` : '';
  return `<div class="contribution-item">
    <div class="contribution-item-main"><strong>${escHtml(term)}</strong> <span class="leaderboard-pill">${escHtml(lang)}</span><br>${escHtml(typeLabel)} · ${escHtml(date)} · ${escHtml(a.source_type || 'source unavailable')}<br>${escHtml(a.definition_preview || '')}${citation}</div>
    <button class="community-link-btn" onclick="openContributionInDefinitionNetwork('${escHtml(a.definition_id || '')}', '${escHtml(term)}', '${escHtml(a.language || '')}')">Open definition ↗</button>
  </div>`;
}

function renderContributorDetail(row) {
  const defs = (row._activities || []).filter(a => a.type === 'definition');
  const anns = (row._activities || []).filter(a => a.type === 'annotation');
  const topTerms = Array.from((row._activities || []).reduce((m,a)=>{ const k=a.term_name||'(term unavailable)'; m.set(k,(m.get(k)||0)+1); return m; }, new Map()).entries()).sort((a,b)=>b[1]-a[1]).slice(0,8);
  return `<div class="contributor-detail open">
    <div class="badge-row">${contributorBadges(row).map(b=>`<span class="community-badge">${escHtml(b)}</span>`).join('')}</div>
    <div class="contributor-card-meta">First contribution: ${row._first_activity ? row._first_activity.toISOString().slice(0,10) : '—'} · Latest contribution: ${row._last_activity ? row._last_activity.toISOString().slice(0,10) : '—'} · Terms contributed to: ${row._term_count}</div>
    <h4 style="margin:.8rem 0 .35rem;color:var(--green-dark);">Definitions submitted (${defs.length})</h4>
    <div class="contribution-list">${defs.length ? defs.map(contributionItemHtml).join('') : '<em>No submitted definitions in this filter.</em>'}</div>
    <h4 style="margin:.8rem 0 .35rem;color:var(--green-dark);">Annotations submitted (${anns.length})</h4>
    <div class="contribution-list">${anns.length ? anns.map(contributionItemHtml).join('') : '<em>No annotations in this filter.</em>'}</div>
    <h4 style="margin:.8rem 0 .35rem;color:var(--green-dark);">Top concepts</h4>
    <div class="badge-row">${topTerms.length ? topTerms.map(([t,n])=>`<span class="leaderboard-pill good">${escHtml(t)} × ${n}</span>`).join('') : '<em>No terms yet.</em>'}</div>
  </div>`;
}

function toggleContributorProfile(contributorId) {
  expandedContributorId = expandedContributorId === contributorId ? null : contributorId;
  renderCommunityDashboard();
}

function switchCommunityView(view) {
  currentCommunityView = view;
  document.querySelectorAll('.community-tab-btn').forEach(b => b.classList.remove('active'));
  const labels = {leaderboard:'Top contributors', recent:'Recent activity', coverage:'Language coverage', cards:'Contributor cards', hall:'Hall of fame'};
  document.querySelectorAll('.community-tab-btn').forEach(b => { if (b.textContent.trim() === labels[view]) b.classList.add('active'); });
  document.querySelectorAll('.community-view').forEach(v => v.classList.remove('active'));
  const pane = document.getElementById(`community-view-${view}`);
  if (pane) pane.classList.add('active');
  renderCommunityDashboard();
}

function renderCommunityDashboard() {
  const rows = getFilteredRows();
  updateLeaderboardSummary(rows);
  renderLeaderboardTable(rows);
  renderRecentActivity(rows);
  renderLanguageCoverage();
  renderContributorCards(rows);
  renderHallOfFame(rows);
}

function renderLeaderboardTable(rows) {
  const container = document.getElementById('leaderboardContent');
  if (!container) return;
  if (!rows.length) { container.innerHTML = '<div class="empty">No matching contributors for this filter.</div>'; return; }
  container.innerHTML = `<table class="leaderboard-table"><thead><tr><th>#</th><th>Contributor</th><th>Definitions</th><th>Annotations</th><th>Languages</th><th>Terms</th><th>Last activity</th><th>Score</th><th>Profile</th></tr></thead><tbody>${rows.map((row,i)=>{
    const medal = i===0?'🥇':i===1?'🥈':i===2?'🥉':i+1;
    const orcid = row._orcid ? `<div class="orcid-link"><a href="https://orcid.org/${escHtml(row._orcid)}" target="_blank" rel="noopener">ORCID: ${escHtml(row._orcid)} ↗</a></div>` : '';
    const affiliation = row.affiliation ? `<div style="font-size:.75rem;color:var(--ink-lt);">${escHtml(row.affiliation)}</div>` : '';
    return `<tr><td><span class="rank-num">${medal}</span></td><td><div class="contributor-name">${escHtml(row._name)}</div>${affiliation}${orcid}</td><td><strong>${row._definitions}</strong></td><td><strong>${row._annotations}</strong></td><td class="lang-counts"><span class="leaderboard-pill good">EN ${row._approved_en}</span><span class="leaderboard-pill">DE ${row._approved_de}</span><span class="leaderboard-pill">ZH ${row._approved_zh}</span></td><td>${row._term_count}</td><td>${row._last_activity ? row._last_activity.toISOString().slice(0,10) : '—'}</td><td><div class="trust-bar"><div class="trust-fill" style="width:${Math.min(100,row._score*8)}%"></div><span class="trust-num">${row._score}</span></div></td><td><button class="leaderboard-action-btn" onclick="switchCommunityView('cards'); expandedContributorId='${escHtml(row.contributor_id)}'; renderCommunityDashboard();">View card</button></td></tr>`;
  }).join('')}</tbody></table>`;
}

function renderRecentActivity(rows) {
  const box = document.getElementById('communityRecentActivity');
  if (!box) return;
  const byId = new Map(rows.map(r => [r.contributor_id, r]));
  const acts = filteredLeaderboardEvents().filter(a => a.contributor_id && byId.has(a.contributor_id)).sort((a,b)=>(parseDateMaybe(b.created_at)?.getTime()||0)-(parseDateMaybe(a.created_at)?.getTime()||0)).slice(0, 50);
  if (!acts.length) { box.innerHTML = '<div class="empty">No recent activity for this filter.</div>'; return; }
  box.innerHTML = `<h2 class="page-title" style="font-size:1.35rem;">Recent Activity</h2><p class="page-sub">A live overview of what contributors have submitted or annotated recently.</p><div class="activity-feed">${acts.map(a=>{
    const row = byId.get(a.contributor_id) || {};
    const who = row._name || a.display_name || 'Unknown contributor';
    const verb = a.type === 'definition' ? 'submitted a definition of' : 'annotated a definition of';
    return `<div class="activity-feed-item"><div><div class="activity-feed-who">${escHtml(who)}</div><div class="activity-feed-date">${shortDate(a.created_at)}</div></div><div class="activity-feed-main"><span class="leaderboard-pill good">${escHtml(a.type)}</span> <span class="leaderboard-pill">${escHtml(langLabel(a.language))}</span><br>${escHtml(verb)} <strong>${escHtml(a.term_name || '(term unavailable)')}</strong><br>${escHtml(a.definition_preview || '')}</div><button class="community-link-btn" onclick="openContributionInDefinitionNetwork('${escHtml(a.definition_id || '')}', '${escHtml(a.term_name || '')}', '${escHtml(a.language || '')}')">Open definition ↗</button></div>`;
  }).join('')}</div>`;
}

function renderLanguageCoverage() {
  const box = document.getElementById('communityLanguageCoverage');
  if (!box) return;
  const events = filteredLeaderboardEvents();
  const buckets = {en:{label:'English',defs:0,ann:0,contributors:new Set(),terms:new Set()}, de:{label:'German',defs:0,ann:0,contributors:new Set(),terms:new Set()}, zh:{label:'Chinese',defs:0,ann:0,contributors:new Set(),terms:new Set()}, other:{label:'Other',defs:0,ann:0,contributors:new Set(),terms:new Set()}};
  events.forEach(e => { const b=buckets[langBucket(e.language)]||buckets.other; if(e.type==='definition') b.defs++; if(e.type==='annotation') b.ann++; if(e.contributor_id) b.contributors.add(e.contributor_id); if(e.term_id||e.term_name) b.terms.add(e.term_id||e.term_name); });
  const maxDefs = Math.max(1, ...Object.values(buckets).map(b=>b.defs));
  box.innerHTML = `<h2 class="page-title" style="font-size:1.35rem;">Language Coverage</h2><p class="page-sub">Definitions, annotations, contributors, and terms covered by language for the selected time range.</p><table class="coverage-table"><thead><tr><th>Language</th><th>Definitions</th><th>Annotations</th><th>Contributors</th><th>Terms</th><th>Coverage</th></tr></thead><tbody>${Object.values(buckets).map(b=>`<tr><td><strong>${escHtml(b.label)}</strong></td><td>${b.defs}</td><td>${b.ann}</td><td>${b.contributors.size}</td><td>${b.terms.size}</td><td><div class="coverage-bar"><div class="coverage-bar-fill" style="width:${Math.round((b.defs/maxDefs)*100)}%"></div></div></td></tr>`).join('')}</tbody></table>`;
}

function renderContributorCards(rows) {
  const box = document.getElementById('communityContributorCards');
  if (!box) return;
  if (!rows.length) { box.innerHTML = '<div class="empty">No contributor cards for this filter.</div>'; return; }
  box.innerHTML = `<h2 class="page-title" style="font-size:1.35rem;">Contributor Cards</h2><p class="page-sub">Each card is a public contribution profile: submitted definitions, annotations, languages, terms, ORCID, and direct links back to definitions.</p><div class="community-grid">${rows.map(row=>{
    const orcid = row._orcid ? `<div class="contributor-card-meta"><a href="https://orcid.org/${escHtml(row._orcid)}" target="_blank" rel="noopener">ORCID ${escHtml(row._orcid)} ↗</a></div>` : '';
    const affiliation = row.affiliation ? `<div class="contributor-card-meta">${escHtml(row.affiliation)}</div>` : '';
    const open = expandedContributorId === row.contributor_id;
    return `<div class="contributor-card"><div class="contributor-card-head"><div><div class="contributor-card-name">${escHtml(row._name)}</div>${affiliation}${orcid}</div><button class="community-link-btn" onclick="toggleContributorProfile('${escHtml(row.contributor_id)}')">${open?'Hide':'View'} profile</button></div><div class="contributor-card-stats"><div class="contributor-card-stat"><strong>${row._definitions}</strong><span>Definitions</span></div><div class="contributor-card-stat"><strong>${row._annotations}</strong><span>Annotations</span></div><div class="contributor-card-stat"><strong>${row._term_count}</strong><span>Terms</span></div></div><div class="badge-row"><span class="leaderboard-pill good">EN ${row._approved_en}</span><span class="leaderboard-pill">DE ${row._approved_de}</span><span class="leaderboard-pill">ZH ${row._approved_zh}</span></div>${open ? renderContributorDetail(row) : ''}</div>`;
  }).join('')}</div>`;
}

function renderHallOfFame(rows) {
  const box = document.getElementById('communityHallOfFame');
  if (!box) return;
  const top = rows.slice().sort((a,b)=>b._score-a._score).slice(0,12);
  if (!top.length) { box.innerHTML = '<div class="empty">No contributors yet.</div>'; return; }
  box.innerHTML = `<h2 class="page-title" style="font-size:1.35rem;">Hall of Fame</h2><p class="page-sub">A non-competitive recognition space for sustained, multilingual, and specialist contributions.</p>
    <div style="background:#f4f4f0;border-radius:8px;padding:.85rem 1rem;margin-bottom:1.25rem;">
      <div style="font-size:.75rem;font-weight:700;color:#1c2b24;margin-bottom:.6rem;">Badge legend</div>
      <div style="display:flex;flex-wrap:wrap;gap:.5rem .75rem;">
        <span style="font-size:.78rem;">🌱 <strong>Contributor</strong> — Has at least one accepted activity</span>
        <span style="font-size:.78rem;">📚 <strong>Definition Curator</strong> — 10+ approved definitions</span>
        <span style="font-size:.78rem;">🏛️ <strong>Senior Curator</strong> — 50+ approved definitions</span>
        <span style="font-size:.78rem;">🔍 <strong>Annotation Specialist</strong> — 25+ annotations</span>
        <span style="font-size:.78rem;">🧠 <strong>Concept Reviewer</strong> — 100+ annotations</span>
        <span style="font-size:.78rem;">🌍 <strong>Bilingual</strong> — Contributes in 2 languages</span>
        <span style="font-size:.78rem;">🌏 <strong>Trilingual</strong> — Contributes in 3 languages</span>
        <span style="font-size:.78rem;">🗺️ <strong>Polyglot</strong> — Contributes in 4+ languages</span>
        <span style="font-size:.78rem;">🧩 <strong>Concept Mapper</strong> — Covers 25+ distinct terms</span>
      </div>
    </div>
    <div class="community-grid">${top.map((row,i)=>{
    const orcid = row._orcid ? `<div class="contributor-card-meta"><a href="https://orcid.org/${escHtml(row._orcid)}" target="_blank" rel="noopener">ORCID ${escHtml(row._orcid)} ↗</a></div>` : '';
    return `<div class="hall-card"><div class="contributor-card-name">${i===0?'🥇 ':i===1?'🥈 ':i===2?'🥉 ':''}${escHtml(row._name)}</div>${orcid}<div class="contributor-card-stats"><div class="contributor-card-stat"><strong>${row._definitions}</strong><span>Definitions</span></div><div class="contributor-card-stat"><strong>${row._annotations}</strong><span>Annotations</span></div><div class="contributor-card-stat"><strong>${row._language_count}</strong><span>Languages</span></div></div><div class="badge-row">${contributorBadges(row).map(b=>`<span class="community-badge">${escHtml(b)}</span>`).join('')}</div><button class="community-link-btn" onclick="switchCommunityView('cards'); expandedContributorId='${escHtml(row.contributor_id)}'; renderCommunityDashboard();">View contribution profile</button></div>`;
  }).join('')}</div>`;
}

// ── Navigation ────────────────────────────────────────────────
function showPage(name, evt) {
  document.querySelectorAll('.page').forEach(p => p.classList.remove('active'));
  document.querySelectorAll('.nav-btn').forEach(b => b.classList.remove('active'));
  const pg = document.getElementById(`page-${name}`);
  if (pg) pg.classList.add('active');
  const navMap = {browse:'Home',dataset:'Dataset',words:'Word-Level',definitions:'Definitions',terms:'Terms',videos:'Videos',leaderboard:'Contributors',about:'About'};
  const label = navMap[name];
  if (label) document.querySelectorAll('.nav-btn').forEach(b => { if (b.textContent.trim()===label) b.classList.add('active'); });
  if (name==='leaderboard') loadLeaderboard();
  history.replaceState(null, '', `#${name}`);
}

function openShinyTab(tab, evt) {
  // Legacy helper retained for backwards compatibility.
  // The main navigation now opens static pages instead of Shiny iframes.
  const pageMap = { words: 'words', definitions: 'definitions', terms: 'terms', videos: 'videos' };
  showPage(pageMap[tab] || 'dataset', evt);
}

function handleUrlParams() {
  handleSmartDeepLink();
}
window.addEventListener('hashchange', handleSmartDeepLink);

// ── Modal ─────────────────────────────────────────────────────
function openModal(tab) {
  const bd = document.getElementById('modalBackdrop');
  bd.style.display = 'flex'; bd.classList.add('open');
}
function closeModal() {
  const bd = document.getElementById('modalBackdrop');
  bd.style.display = 'none'; bd.classList.remove('open');
}
function closeModalOutside(e) { if (e.target===document.getElementById('modalBackdrop')) closeModal(); }
function switchTab(tab) {
  document.getElementById('formLogin').style.display = tab==='login'?'block':'none';
}

// ── Forgot password ───────────────────────────────────────────
function showForgotPassword(e) {
  e.preventDefault();
  document.getElementById('formLogin').style.display = 'none';
  document.getElementById('formForgot').style.display = 'block';
  document.getElementById('modalTitle').textContent = 'Reset your password';
  document.getElementById('modalSub').textContent = "We'll send you a link to set a new password.";
  document.getElementById('modalTabs').style.display = 'none';
}
function showLoginForm(e) {
  e.preventDefault();
  document.getElementById('formLogin').style.display = 'block';
  document.getElementById('formForgot').style.display = 'none';
  document.getElementById('modalTitle').textContent = 'Welcome back';
  document.getElementById('modalSub').textContent = 'Log in to contribute definitions and annotate terms.';
  document.getElementById('modalTabs').style.display = 'flex';
}
async function doForgotPassword() {
  const email = document.getElementById('forgotEmail').value.trim();
  const msg = document.getElementById('forgotMsg');
  const btn = document.getElementById('btnForgot');
  if (!email) { showMsg(msg, 'Please enter your email address.', 'error'); return; }
  btn.disabled = true; btn.textContent = 'Sending...';
  const { error } = await supa.auth.resetPasswordForEmail(email, { redirectTo: window.location.origin + window.location.pathname });
  btn.disabled = false; btn.textContent = 'Send reset link';
  if (error) { showMsg(msg, 'Error: ' + error.message, 'error'); }
  else { showMsg(msg, '✓ Reset link sent — check your email.', 'success'); btn.textContent = 'Sent ✓'; }
}

// ── Auth callback ─────────────────────────────────────────────
async function handleAuthCallback() {
  try {
    const hash = window.location.hash;
    if (!hash || !hash.includes('access_token') || !supa) return;
    const params = new URLSearchParams(hash.replace('#', ''));
    const type = params.get('type');
    const accessToken = params.get('access_token');
    const refreshToken = params.get('refresh_token');
    if (!accessToken) return;
    if (['invite','recovery','signup'].includes(type)) {
      const { error } = await supa.auth.setSession({ access_token: accessToken, refresh_token: refreshToken||'' });
      if (error) { console.error('Auth callback error:', error); return; }
      history.replaceState(null, '', window.location.pathname);
      if (type==='invite' || type==='recovery') openPasswordSetModal();
      else await checkAuth();
    }
  } catch(e) { console.warn('Auth callback error:', e); }
}

function openPasswordSetModal() {
  document.getElementById('passwordSetBackdrop').style.display = 'flex';
  document.getElementById('passwordSetBackdrop').classList.add('open');
}
async function doSetPassword() {
  const pw1 = document.getElementById('newPassword').value;
  const pw2 = document.getElementById('confirmPassword').value;
  const msg = document.getElementById('passwordSetMsg');
  const btn = document.getElementById('btnSetPassword');
  if (!pw1 || pw1.length < 8) { showMsg(msg, 'Password must be at least 8 characters.', 'error'); return; }
  if (pw1 !== pw2) { showMsg(msg, 'Passwords do not match.', 'error'); return; }
  btn.disabled = true; btn.textContent = 'Activating...';
  const { error } = await supa.auth.updateUser({ password: pw1 });
  if (error) { showMsg(msg, 'Error: ' + error.message, 'error'); btn.disabled=false; btn.textContent='Activate account'; return; }
  showMsg(msg, '✓ Account activated! Welcome to Re-SearchTerms.', 'success');
  btn.textContent = 'Done ✓';
  setTimeout(async () => { document.getElementById('passwordSetBackdrop').classList.remove('open'); document.getElementById('passwordSetBackdrop').style.display='none'; await checkAuth(); }, 2000);
}

// ── Utilities ─────────────────────────────────────────────────
function escHtml(str) { if (!str) return ''; return str.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;'); }
function showMsg(el, text, type) { el.innerHTML = `<div class="form-${type==='error'?'error':'success'}">${text}</div>`; }

// ── Verify definition (source check = starred) ───────────────
async function verifyDefinition(event, defId, btn) {
  if (event) event.stopPropagation();
  if (!currentUser) { openTallyModal(); return; }
  const { data: { session } } = await supa.auth.getSession();
  const resp = await fetch(`${SUPABASE_URL}/rest/v1/definitions?id=eq.${defId}`, {
    method: 'PATCH',
    headers: {
      'apikey': SUPABASE_ANON_KEY,
      'Authorization': `Bearer ${session.access_token}`,
      'Content-Type': 'application/json',
      'Prefer': 'return=minimal'
    },
    body: JSON.stringify({
      validation_status: 'source_verified',
      verified_by: currentUser.id
    })
  });
  if (resp.ok) {
    // Re-open panel to refresh
    const termId = document.querySelector('.panel-term-name')?.dataset?.termId;
    alert('✓ Definition marked as source-verified. Thank you!');
  } else {
    alert('Could not verify — you may not have permission yet.');
  }
}

// ── Contribution form logic ─────────────────────────────────
function splitSemiColon(x) { return String(x || '').split(';').map(s => s.trim()).filter(Boolean); }

function getVal(id) {
  const el = document.getElementById(id);
  return el ? el.value.trim() : '';
}

function getCheckedValues(containerId) {
  const box = document.getElementById(containerId);
  if (!box) return [];
  return Array.from(box.querySelectorAll('input[type="checkbox"]:checked')).map(x => x.value);
}

function setIfExists(obj, key, value) {
  if (value !== undefined && value !== null && value !== '') obj[key] = value;
}


function updateContribLanguageHelpers() {
  const lang = getVal('contribLang') || 'en';
  const L = 'en'; // UI always in English regardless of submission language
  const text = {
    en: {
      sourceTermLabel: 'e.g. scientific impact, if the selected database term is academic impact',
      sourceTermLabelHint: 'Use this to capture jingle-jangle cases: the source may define the same or a closely related concept using a different label.',
      originalSourceWording: 'Paste the definition in the original language as it appears in the source.',
      originalSourceWordingHint: 'Required if you submit your own translation. This helps bilingual validators compare your translation with the source text.',
      contribText: 'Paste the definition. Keep in-text citations such as (Smith, 2014) if they appear in the source definition.',
      definitionTextHelp: '<strong>Important:</strong> include citations that appear inside the definition, for example “Open science refers to … (Fecher & Friesike, 2014).” Do not remove parenthetical citations because they are useful for provenance and future citation-network analyses.',
      sourceLocationTypeHint: 'This is not the source type. It tells validators where inside the source the definition appears.',
      sourceLocation: 'e.g. p. 35; Section 2.1; Slide 18; Entry: Open Science',
      fullCitation: 'Paste or type the full citation as completely as possible.',
      citAuthor: 'Surname, Initials.; Surname, Initials. e.g. Smith, J.; Jones, A. B.',
      authorFormatHint: 'Preferred format: Surname, Initials.; separate multiple authors with semicolons. If unsure, paste the author list as shown by the source.',
      citTitle: 'e.g. Open Science: A Practical Guide',
      publicationOutlet: 'Journal, publisher, institution, website, organisation...',
      citIsbn: 'For books/textbooks',
      disciplineOther: 'If Other, please specify',
      researchContextOther: 'If Other, please specify',
      contributorNote: 'e.g. Useful when discussing transparency in quantitative psychology, but perhaps too narrow for interdisciplinary work.',
      definitionTypeHint: 'Own synthesis or paraphrase is not allowed. If you translate it yourself, you must also provide the original source wording below.'
    },
    de: {
      sourceTermLabel: 'z. B. wissenschaftlicher Einfluss, wenn der Datenbankbegriff akademischer Einfluss lautet',
      sourceTermLabelHint: 'Damit erfassen wir Jingle-Jangle-Fälle: Die Quelle kann dasselbe oder ein ähnliches Konzept mit einer anderen Bezeichnung definieren.',
      originalSourceWording: 'Fügen Sie die Definition in der Originalsprache genau so ein, wie sie in der Quelle erscheint.',
      originalSourceWordingHint: 'Erforderlich, wenn Sie eine eigene Übersetzung einreichen. So können zweisprachige Prüfer:innen Ihre Übersetzung mit dem Original vergleichen.',
      contribText: 'Fügen Sie die Definition ein. Behalten Sie Quellenangaben im Text wie (Smith, 2014) bei, wenn sie in der Definition erscheinen.',
      definitionTextHelp: '<strong>Wichtig:</strong> Behalten Sie Quellenangaben bei, die innerhalb der Definition erscheinen, z. B. „Open Science bezeichnet … (Fecher & Friesike, 2014).“ Entfernen Sie solche Klammerzitate nicht, da sie für Provenienz- und Zitationsnetzwerkanalysen nützlich sind.',
      sourceLocationTypeHint: 'Dies ist nicht der Quellentyp. Dieses Feld zeigt Prüfer:innen, wo genau die Definition innerhalb der Quelle steht.',
      sourceLocation: 'z. B. S. 35; Abschnitt 2.1; Folie 18; Eintrag: Open Science',
      fullCitation: 'Fügen Sie die vollständige Literaturangabe so vollständig wie möglich ein.',
      citAuthor: 'Nachname, Initialen; Nachname, Initialen; z. B. Schmidt, A.; Müller, B. C.',
      authorFormatHint: 'Bevorzugtes Format: Nachname, Initialen; mehrere Autor:innen mit Semikolon trennen. Wenn unsicher, fügen Sie die Autor:innen so ein, wie sie in der Quelle stehen.',
      citTitle: 'z. B. Open Science: Eine praktische Einführung',
      publicationOutlet: 'Zeitschrift, Verlag, Institution, Website, Organisation...',
      citIsbn: 'Für Bücher/Lehrbücher',
      disciplineOther: 'Wenn „Andere“, bitte angeben',
      researchContextOther: 'Wenn „Andere“, bitte angeben',
      contributorNote: 'z. B. Nützlich für Diskussionen über Transparenz in der quantitativen Psychologie, aber möglicherweise zu eng für interdisziplinäre Arbeiten.',
      definitionTypeHint: 'Eigene Synthesen oder Paraphrasen sind nicht erlaubt. Wenn Sie selbst übersetzen, müssen Sie unten auch den Originalwortlaut der Quelle angeben.'
    },
    'zh-hans': {
      sourceTermLabel: '例如：如果数据库术语是“学术影响”，来源使用的是“科学影响”，请在此填写',
      sourceTermLabelHint: '用于记录 jingle-jangle 情况：来源可能用不同标签定义相同或相近的概念。',
      originalSourceWording: '请粘贴来源中的原文定义，保持原语言和原措辞。',
      originalSourceWordingHint: '如果你提交的是自己翻译的定义，此项为必填，方便双语验证者核对译文和原文。',
      contribText: '请粘贴定义文本。如果定义中本身含有文内引用（如 Smith, 2014），请保留。',
      definitionTextHelp: '<strong>重要：</strong>请保留定义中出现的文内引用，例如“开放科学指……（Fecher & Friesike, 2014）”。这些括号引用有助于后续追踪定义来源和进行引文网络分析。',
      sourceLocationTypeHint: '这不是来源类型，而是告诉验证者定义在来源中的具体位置。',
      sourceLocation: '例如：第35页；第2.1节；第18张幻灯片；词条：开放科学',
      fullCitation: '请尽可能完整地粘贴或输入参考文献信息。',
      citAuthor: '中文来源：请按来源显示填写全名，如：陈大文；李小明。外文来源：Smith, J.; Wang, X.',
      authorFormatHint: '如果来源本身是中文出版物，请直接填写中文作者全名（姓氏+名字），多位作者用分号分隔。如果来源是外文出版物，请使用外文参考文献格式，例如：Smith, J.; Wang, X.。',
      citTitle: '例如：开放科学实践指南',
      publicationOutlet: '期刊、出版社、机构、网站、组织……',
      citIsbn: '适用于书籍/教材',
      disciplineOther: '如果选择“其他”，请说明',
      researchContextOther: '如果选择“其他”，请说明',
      contributorNote: '例如：适合讨论定量心理学中的透明性，但对于跨学科语境可能过窄。',
      definitionTypeHint: '不允许提交自己的综合定义或改写。如果你自己翻译定义，必须在下方提供来源中的原文。'
    },
    'zh-hant': {
      sourceTermLabel: '例如：如果資料庫術語是「學術影響」，來源使用的是「科學影響」，請在此填寫',
      sourceTermLabelHint: '用於記錄 jingle-jangle 情況：來源可能用不同標籤定義相同或相近的概念。',
      originalSourceWording: '請貼上來源中的原文定義，保留原語言和原措辭。',
      originalSourceWordingHint: '如果你提交的是自己翻譯的定義，此欄為必填，方便雙語驗證者核對譯文和原文。',
      contribText: '請貼上定義文本。如果定義中本身含有文內引用（如 Smith, 2014），請保留。',
      definitionTextHelp: '<strong>重要：</strong>請保留定義中出現的文內引用，例如「開放科學指……（Fecher & Friesike, 2014）」。這些括號引用有助於後續追蹤定義來源和進行引文網絡分析。',
      sourceLocationTypeHint: '這不是來源類型，而是告訴驗證者定義在來源中的具體位置。',
      sourceLocation: '例如：第35頁；第2.1節；第18張投影片；詞條：開放科學',
      fullCitation: '請盡可能完整地貼上或輸入參考文獻資訊。',
      citAuthor: '中文來源：請按來源顯示填寫全名，如：陳大文；李小明。外文來源：Smith, J.; Chan, Y.',
      authorFormatHint: '如果來源本身是中文出版物，請直接填寫中文作者全名（姓氏+名字），多位作者用分號分隔。如果來源是外文出版物，請使用外文參考文獻格式，例如：Smith, J.; Chan, Y.。',
      citTitle: '例如：開放科學實踐指南',
      publicationOutlet: '期刊、出版社、機構、網站、組織……',
      citIsbn: '適用於書籍/教材',
      disciplineOther: '如果選擇「其他」，請說明',
      researchContextOther: '如果選擇「其他」，請說明',
      contributorNote: '例如：適合討論定量心理學中的透明性，但對於跨學科語境可能過窄。',
      definitionTypeHint: '不允許提交自己的綜合定義或改寫。如果你自己翻譯定義，必須在下方提供來源中的原文。'
    }
  }[L];

  const setPh = (id, val) => { const el = document.getElementById(id); if (el) el.placeholder = val; };
  const setHtml = (id, val) => { const el = document.getElementById(id); if (el) el.innerHTML = val; };
  const setText = (id, val) => { const el = document.getElementById(id); if (el) el.textContent = val; };

  setPh('sourceTermLabel', text.sourceTermLabel);
  setText('sourceTermLabelHint', text.sourceTermLabelHint);
  setPh('originalSourceWording', text.originalSourceWording);
  setText('originalSourceWordingHint', text.originalSourceWordingHint);
  setPh('contribText', text.contribText);
  setHtml('definitionTextHelp', text.definitionTextHelp);
  setText('sourceLocationTypeHint', text.sourceLocationTypeHint);
  setPh('sourceLocation', text.sourceLocation);
  setPh('fullCitation', text.fullCitation);
  setPh('citAuthor', text.citAuthor);
  setText('authorFormatHint', text.authorFormatHint);
  setPh('citTitle', text.citTitle);
  setPh('publicationOutlet', text.publicationOutlet);
  setPh('citIsbn', text.citIsbn);
  setPh('disciplineOther', text.disciplineOther);
  setPh('researchContextOther', text.researchContextOther);
  setPh('contributorNote', text.contributorNote);
  setText('definitionTypeHint', text.definitionTypeHint);
}

function syncNewTermName() {
  const input = document.getElementById('newTermName');
  const termInput = document.getElementById('contribTerm');
  if (input && termInput && pendingContributeTerm?.isNewTerm) termInput.value = input.value.trim() || '(new term pending review)';
}

function setNewTermMode(isNew) {
  const box = document.getElementById('newTermFields');
  const termInput = document.getElementById('contribTerm');
  if (box) box.style.display = isNew ? 'block' : 'none';
  if (termInput) {
    termInput.readOnly = true;
    termInput.style.background = '#f4f4f0';
  }
}

function openNewTermContrib(event, suggestedName) {
  if (event) event.stopPropagation();
  if (!currentUser) {
    openModal('login');
    const msg = document.getElementById('loginMsg');
    if (msg) showMsg(msg, 'Please log in before suggesting a new term. New contributors can apply through the sign-up form.', 'error');
    return;
  }
  pendingContributeTerm = { id: null, name: suggestedName || '', isNewTerm: true };
  setNewTermMode(true);
  const nameInput = document.getElementById('newTermName');
  if (nameInput) nameInput.value = suggestedName || '';
  const termInput = document.getElementById('contribTerm');
  if (termInput) termInput.value = suggestedName || '(new term pending review)';
  const msg = document.getElementById('contribMsg');
  if (msg) msg.innerHTML = '';
  document.getElementById('contribModeSubmit').style.display = 'block';
  document.getElementById('contribModeAnnotate').style.display = 'none';
  document.getElementById('contribBackdrop').style.display = 'flex';
  document.getElementById('contribBackdrop').classList.add('open');
  document.body.classList.add('modal-open');
  updateContribLanguageHelpers();
}

function openContrib(event, termId, termName, presetLang) {
  if (event) event.stopPropagation();
  if (!currentUser) {
    openModal('login');
    const msg = document.getElementById('loginMsg');
    if (msg) showMsg(msg, 'Please log in before submitting a definition. New contributors can apply through the sign-up form.', 'error');
    return;
  }
  pendingContributeTerm = { id: termId, name: termName, isNewTerm: false };
  setNewTermMode(false);
  ['newTermName','newTermSynonyms','newTermRelatedExisting','newTermReason'].forEach(id => { const el = document.getElementById(id); if (el) el.value = ''; });
  const termInput = document.getElementById('contribTerm');
  if (termInput) termInput.value = termName || '';
  const langSel = document.getElementById('contribLang');
  if (langSel && presetLang) langSel.value = presetLang;
  updateContribLanguageHelpers();
  const msg = document.getElementById('contribMsg');
  if (msg) msg.innerHTML = '';
  document.getElementById('contribModeSubmit').style.display = 'block';
  document.getElementById('contribModeAnnotate').style.display = 'none';
  document.getElementById('contribBackdrop').style.display = 'flex';
  document.getElementById('contribBackdrop').classList.add('open');
  document.body.classList.add('modal-open');
}

function closeContrib() {
  const bd = document.getElementById('contribBackdrop');
  bd.style.display = 'none';
  bd.classList.remove('open');
  document.body.classList.remove('modal-open');
}

function closeContribOutside(e) {
  if (e.target === document.getElementById('contribBackdrop')) closeContrib();
}

function updateSourceTypeHint() {
  const sourceType = getVal('contribSourceType');
  const hint = document.getElementById('sourceTypeHint');
  if (!hint) return;
  const hints = {
    journal_article: 'Journal articles usually have a DOI. Please also provide journal name, year, and page/section location.',
    book: 'Books usually have an ISBN. Please provide publisher, edition if relevant, and exact page number(s).',
    book_chapter: 'Please provide chapter title, book title, editors if known, publisher, and page number(s).',
    textbook: 'Please provide edition, publisher, ISBN if available, and page number(s).',
    dictionary: 'Please provide dictionary title, edition/version, entry title, and URL or ISBN if available.',
    glossary: 'Please provide glossary name, organisation/source, URL if available, and entry title.',
    website: 'Please provide URL, organisation/website name, and access date.',
    report: 'Please provide organisation/publisher, report number if available, URL/DOI if available, and page number(s).',
    policy_document: 'Please provide organisation, document title, URL if available, and section/page location.',
    conference_paper: 'Please provide conference name, location/date if available, DOI/URL if available, and page/section location.',
    slides: 'Slides may not have DOI/URL. Please provide presenter, institution/event/course, year, and slide number.',
    teaching_material: 'Please provide author/instructor, institution/course/workshop, year, and page/slide/section location.',
    other: 'Please provide enough citation details for another contributor to locate and verify the definition.'
  };
  hint.textContent = hints[sourceType] || hints.other;
}

function updateTranslationFields() {
  const type = getVal('definitionType');
  const fields = document.getElementById('translationFields');
  const req = document.getElementById('originalWordingRequired');
  const show = ['source_provided_translation','contributor_translation_of_source'].includes(type);
  if (fields) fields.style.display = show ? 'block' : 'none';
  if (req) req.style.display = type === 'contributor_translation_of_source' ? 'inline' : 'none';
}

function updateProvenanceFields() {
  const provenance = getVal('definitionProvenance');
  const show = ['direct_quote_from_cited_source','adapted_from_cited_source','synthesises_multiple_cited_sources'].includes(provenance);
  const fields = document.getElementById('originalSourceFields');
  if (fields) fields.style.display = show ? 'block' : 'none';
}

function addOriginalSource() {
  const container = document.getElementById('originalSourcesContainer');
  if (!container) return;
  const n = container.querySelectorAll('.original-source-item').length + 1;
  const div = document.createElement('div');
  div.className = 'original-source-item';
  div.dataset.sourceIndex = String(n);
  div.innerHTML = `
    <button type="button" class="remove-row-btn" onclick="removeOriginalSource(this)">Remove</button>
    <div style="font-size:.75rem;font-weight:700;color:var(--green-dark);margin-bottom:.55rem;">Earlier cited source ${n}</div>
    <div class="form-grid-2">
      <div class="form-group"><label class="form-label">Original source author(s)</label><input class="form-input orig-author" type="text" placeholder="Surname, Initials.; Surname, Initials." /></div>
      <div class="form-group"><label class="form-label">Original source year</label><input class="form-input orig-year" type="number" placeholder="2014" min="1500" max="2100" /></div>
    </div>
    <div class="form-group"><label class="form-label">Original source title</label><input class="form-input orig-title" type="text" placeholder="Title of the source cited by the current source" /></div>
    <div class="form-grid-3">
      <div class="form-group"><label class="form-label">Original DOI</label><input class="form-input orig-doi" type="text" placeholder="10.xxxx/xxxxx" /></div>
      <div class="form-group"><label class="form-label">Original ISBN</label><input class="form-input orig-isbn" type="text" /></div>
      <div class="form-group"><label class="form-label">Original URL</label><input class="form-input orig-url" type="url" placeholder="https://..." /></div>
    </div>
    <div class="form-group"><label class="form-label">How is this earlier source used?</label><select class="form-select orig-relation"><option value="cited_as_definition_source">Cited as definition source</option><option value="directly_quoted">Directly quoted</option><option value="adapted_or_paraphrased">Adapted or paraphrased</option><option value="one_of_multiple_sources">One of multiple sources</option><option value="unclear">Unclear</option></select></div>
    <label class="check-row"><input type="checkbox" class="orig-verified" /> I personally checked this original cited source.</label>
  `;
  container.appendChild(div);
}

function removeOriginalSource(btn) {
  const item = btn.closest('.original-source-item');
  if (item) item.remove();
}

function collectOriginalSources() {
  const container = document.getElementById('originalSourcesContainer');
  if (!container) return [];
  return Array.from(container.querySelectorAll('.original-source-item')).map(item => {
    const q = cls => (item.querySelector(cls)?.value || '').trim();
    return {
      authors: q('.orig-author'),
      year: q('.orig-year') ? Number(q('.orig-year')) : null,
      title: q('.orig-title'),
      doi: q('.orig-doi').replace(/^https?:\/\/(dx\.)?doi\.org\//i, ''),
      isbn: q('.orig-isbn'),
      url: q('.orig-url'),
      relation: q('.orig-relation'),
      verified_by_submitter: !!item.querySelector('.orig-verified')?.checked
    };
  }).filter(src => src.authors || src.year || src.title || src.doi || src.isbn || src.url);
}

function resetDoiVerify() {
  const status = document.getElementById('doiStatus');
  if (status) status.innerHTML = '';
}

async function verifyDoi() {
  const doi = getVal('citDoi').replace(/^https?:\/\/(dx\.)?doi\.org\//i, '');
  const status = document.getElementById('doiStatus');
  if (!doi) { if (status) status.innerHTML = '<span style="color:#c0392b">Please enter a DOI first.</span>'; return; }
  if (status) status.innerHTML = '<span style="color:var(--ink-lt)">Checking DOI metadata…</span>';
  try {
    const res = await fetch('https://api.crossref.org/works/' + encodeURIComponent(doi));
    if (!res.ok) throw new Error('DOI not found');
    const json = await res.json();
    const m = json.message || {};
    const title = (m.title && m.title[0]) || '';
    const year = m.published?.['date-parts']?.[0]?.[0] || m.created?.['date-parts']?.[0]?.[0] || '';
    const authors = (m.author || []).map(a => [a.family, a.given].filter(Boolean).join(', ')).join('; ');
    const outlet = (m['container-title'] && m['container-title'][0]) || m.publisher || '';
    if (title && !getVal('citTitle')) document.getElementById('citTitle').value = title;
    if (year && !getVal('citYear')) document.getElementById('citYear').value = year;
    if (authors && !getVal('citAuthor')) document.getElementById('citAuthor').value = authors;
    if (outlet && !getVal('publicationOutlet')) document.getElementById('publicationOutlet').value = outlet;
    if (status) status.innerHTML = '<span style="color:#1a7a4a">✓ DOI found. Metadata fields were filled where empty.</span>';
  } catch (e) {
    if (status) status.innerHTML = '<span style="color:#c0392b">Could not verify this DOI automatically. You may still submit with a full citation.</span>';
  }
}

function validateContributionForm() {
  const msg = document.getElementById('contribMsg');
  const required = [
    ['contribTerm', 'Term is missing.'],
    ['contribLang', 'Please select a language.'],
    ['definitionType', 'Please select a definition wording type.'],
    ['contribText', 'Please paste the definition text.'],
    ['sourceLocationType', 'Please select where another person can find the definition inside the source.'],
    ['sourceLocation', 'Please provide the exact page, section, slide, entry, or other locator.'],
    ['contribSourceType', 'Please select a source type.'],
    ['fullCitation', 'Please provide a full citation.'],
    ['citAuthor', 'Please provide the author(s) of the current source.'],
    ['citYear', 'Please provide the year of the current source.'],
    ['citTitle', 'Please provide the title of the current source.'],
    ['sourceAccessibility', 'Please select source accessibility.'],
    ['definitionProvenance', 'Please indicate definition provenance.'],
    ['selfConfidence', 'Please rate your confidence that the definition was copied or translated accurately.'],
    ['metadataConfidence', 'Please rate your confidence in the metadata.']
  ];
  for (const [id, message] of required) {
    if (!getVal(id)) { showMsg(msg, message, 'error'); return false; }
  }

  if (pendingContributeTerm?.isNewTerm) {
    if (!getVal('newTermName')) { showMsg(msg, 'Please provide the proposed new term name.', 'error'); return false; }
    if (!getVal('newTermLanguage')) { showMsg(msg, 'Please select the language of the proposed term.', 'error'); return false; }
  }

  const defType = getVal('definitionType');
  if (defType === 'contributor_translation_of_source' && !getVal('originalSourceWording')) {
    showMsg(msg, 'Because this is your translation, please paste the original source wording too.', 'error');
    return false;
  }

  const defText = getVal('contribText');
  if (defText.length < 20) { showMsg(msg, 'The definition text is very short. Please check that you pasted the full definition.', 'error'); return false; }
  if (defText.length > 6000) { showMsg(msg, 'The definition text is very long. Please submit only the definition, not the whole source passage.', 'error'); return false; }

  const year = Number(getVal('citYear'));
  if (!Number.isFinite(year) || year < 1500 || year > 2100) { showMsg(msg, 'Please enter a plausible publication year.', 'error'); return false; }

  const provenance = getVal('definitionProvenance');
  if (['direct_quote_from_cited_source','adapted_from_cited_source','synthesises_multiple_cited_sources'].includes(provenance)) {
    const originalSources = collectOriginalSources();
    if (!originalSources.length) {
      showMsg(msg, 'Because the current source uses earlier cited source(s), please add at least one earlier cited source.', 'error');
      return false;
    }
  }

  if (getCheckedValues('disciplineTags').includes('other') && !getVal('disciplineOther')) {
    showMsg(msg, 'Please specify the discipline because you selected Other.', 'error'); return false;
  }
  if (getCheckedValues('researchContextTags').includes('other') && !getVal('researchContextOther')) {
    showMsg(msg, 'Please specify the research context because you selected Other.', 'error'); return false;
  }

  const checks = ['confirmExactCopy','confirmCitationsKept','confirmNoSynthesis','confirmMetadata','confirmLocation','confirmReview'];
  for (const id of checks) {
    const el = document.getElementById(id);
    if (!el || !el.checked) { showMsg(msg, 'Please complete all verification checklist items before submitting.', 'error'); return false; }
  }
  return true;
}

function previewScreenshot(input) {
  const preview = document.getElementById('screenshotPreview');
  const status = document.getElementById('screenshotUploadStatus');
  if (!preview) return;
  const file = input.files[0];
  if (!file) { preview.innerHTML = ''; return; }
  if (file.size > 5 * 1024 * 1024) {
    if (status) status.innerHTML = '<span style="color:#c0392b;">File too large. Maximum 5 MB.</span>';
    input.value = ''; preview.innerHTML = ''; return;
  }
  if (status) status.innerHTML = '';
  if (file.type.startsWith('image/')) {
    const reader = new FileReader();
    reader.onload = e => {
      preview.innerHTML = '<img src="' + e.target.result + '" style="max-width:100%;max-height:200px;border-radius:6px;border:1px solid #d6e4dc;margin-top:.4rem;" alt="Screenshot preview" />';
    };
    reader.readAsDataURL(file);
  } else {
    preview.innerHTML = '<div style="font-size:.78rem;color:#4a5e54;padding:.4rem;">📄 ' + file.name + ' (' + Math.round(file.size/1024) + ' KB)</div>';
  }
}

async function verifyAndAutofillDoi() {
  const doi = document.getElementById('citDoi') ? document.getElementById('citDoi').value.trim().replace(/^https?:\/\/(dx\.)?doi\.org\//i,'') : '';
  const status = document.getElementById('doiStatus');
  const btn = document.getElementById('btnVerifyDoi');
  if (!doi) { if (status) status.innerHTML = '<span style="color:#c0392b">Please enter a DOI first.</span>'; return; }
  btn.textContent = 'Checking...'; btn.disabled = true;
  try {
    const res = await fetch('https://api.crossref.org/works/' + encodeURIComponent(doi));
    if (res.ok) {
      const d = await res.json();
      const work = d.message;
      const setIfEmpty = (id, val) => { const el = document.getElementById(id); if (el && !el.value && val) el.value = val; };
      if (work.title && work.title[0]) setIfEmpty('citTitle', work.title[0]);
      if (work.author && work.author.length) setIfEmpty('citAuthor', work.author.map(a => (a.family||'') + (a.given ? ', ' + a.given[0] + '.' : '')).join('; '));
      const yr = work.published && work.published['date-parts'] && work.published['date-parts'][0] && work.published['date-parts'][0][0];
      if (yr) setIfEmpty('citYear', yr);
      setIfEmpty('publicationOutlet', (work['container-title'] && work['container-title'][0]) || work.publisher || '');
      doiVerified = true;
      if (status) status.innerHTML = '<span style="color:#1a7a4a;font-weight:600;">✓ DOI verified — fields auto-filled where empty</span>';
    } else {
      doiVerified = false;
      if (status) status.innerHTML = '<span style="color:#c0392b;">✗ DOI not found — please fill fields manually</span>';
    }
  } catch(e) {
    doiVerified = false;
    if (status) status.innerHTML = '<span style="color:#8aa09a;">Could not verify — please fill fields manually</span>';
  }
  btn.textContent = 'Check & fill'; btn.disabled = false;
}


// ── Cluster filters for word-level and definition-level ──────
function populateClusterFilters() {
  if (!window.termLevelData || !termLevelData.cluster_points) return;
  const clusters = [...new Set(termLevelData.cluster_points.map(p => p.cluster_name).filter(Boolean))].sort();
  const opts = '<option value="">All clusters</option>' + clusters.map(c =>
    '<option value="' + escapeHtml(c) + '">' + escapeHtml(c) + '</option>'
  ).join('');
  const wf = document.getElementById('wordClusterFilter');
  const df = document.getElementById('definitionClusterFilter');
  if (wf) wf.innerHTML = opts;
  if (df) df.innerHTML = opts;
}

function getTermsForCluster(clusterValue) {
  if (!clusterValue || !window.termLevelData) return null;
  return termLevelData.cluster_points
    .filter(p => p.cluster_name === clusterValue)
    .map(p => p.term);
}

function filterWordTermsByCluster() {
  const cluster = document.getElementById('wordClusterFilter')?.value || '';
  const sel = document.getElementById('wordTermSelect');
  if (!sel) return;
  const terms = cluster ? getTermsForCluster(cluster) : null;
  // Re-populate term select filtered by cluster
  const allTermKeys = Object.keys(wordLevelData || {}).sort((a,b) => a.localeCompare(b));
  const filtered = terms ? allTermKeys.filter(t => terms.some(ct => ct.toLowerCase() === t.toLowerCase())) : allTermKeys;
  sel.innerHTML = filtered.map(t => '<option value="' + escapeHtml(t) + '">' + escapeHtml(t) + '</option>').join('');
  if (filtered.length) { sel.value = filtered[0]; renderWordLevel(); }
}

function filterDefTermsByCluster() {
  const cluster = document.getElementById('definitionClusterFilter')?.value || '';
  const sel = document.getElementById('definitionTermSelect');
  if (!sel) return;
  const terms = cluster ? getTermsForCluster(cluster) : null;
  const legacyTerms = [...new Set(legacyDefinitionNodes.map(d => String(d.concept || '').trim()).filter(Boolean))];
  const supaTerms = (window.allTerms || allTerms || []).map(t => t.name_en).filter(Boolean);
  const allT = [...new Set([...supaTerms, ...legacyTerms])].sort((a,b) => a.localeCompare(b));
  const filtered = terms ? allT.filter(t => terms.some(ct => ct.toLowerCase() === t.toLowerCase())) : allT;
  sel.innerHTML = '<option value="">Select a term…</option>' + filtered.map(t =>
    '<option value="' + escapeHtml(t) + '">' + escapeHtml(t) + '</option>'
  ).join('');
}

// ── Contribute from definition page ──────────────────────────
function openContribFromDefPage(lang) {
  const termName = document.getElementById('definitionTermSelect')?.value || '';
  if (!termName) return;
  const termObj = (window.allTerms || allTerms || []).find(t =>
    t.name_en && t.name_en.toLowerCase() === termName.toLowerCase()
  );
  const termId = termObj?.id || termName;
  openContrib(null, termId, termName, lang);
}

// Show/update the contribution strip when a term is selected in definition page
function updateDefinitionContribStrip() {
  const termName = document.getElementById('definitionTermSelect')?.value || '';
  const strip = document.getElementById('definitionContribStrip');
  const nameEl = document.getElementById('definitionContribTermName');
  if (!strip) return;
  if (termName) {
    strip.style.display = 'flex';
    if (nameEl) nameEl.textContent = termName;
  } else {
    strip.style.display = 'none';
  }
}

async function submitContribution() {
  const msg = document.getElementById('contribMsg');
  const btn = document.getElementById('btnSubmitContrib');
  if (!currentUser) { openModal('login'); return; }
  if (!validateContributionForm()) return;

  btn.disabled = true;
  btn.textContent = 'Submitting…';

  const payload = {
    term_id: pendingContributeTerm?.id || null,
    source_term_label: getVal('sourceTermLabel'),
    language: getVal('contribLang'),
    definition_text: getVal('contribText'),
    definition_type: getVal('definitionType'),
    original_source_wording: getVal('originalSourceWording'),
    source_type: getVal('contribSourceType'),
    citation_author: getVal('citAuthor'),
    citation_year: Number(getVal('citYear')),
    citation_title: getVal('citTitle'),
    citation_doi: getVal('citDoi').replace(/^https?:\/\/(dx\.)?doi\.org\//i, ''),
    citation_url: getVal('citUrl'),
    citation_isbn: getVal('citIsbn'),
    full_citation: getVal('fullCitation'),
    publication_outlet: getVal('publicationOutlet'),
    access_date: getVal('accessDate') || null,
    source_location_type: getVal('sourceLocationType'),
    source_location: getVal('sourceLocation'),
    source_accessibility: getVal('sourceAccessibility'),
    definition_provenance: getVal('definitionProvenance'),
    original_sources: collectOriginalSources(),
    disciplines: getCheckedValues('disciplineTags'),
    discipline_other: getVal('disciplineOther'),
    research_contexts: getCheckedValues('researchContextTags'),
    research_context_other: getVal('researchContextOther'),
    definition_style: getVal('definitionStyle'),
    definition_scope: getVal('definitionScope'),
    tags: getCheckedValues('selfTags'),
    suitable_contexts: getCheckedValues('suitableContextTags'),
    contributor_note: getVal('contributorNote'),
    self_confidence: Number(getVal('selfConfidence')),
    metadata_confidence: Number(getVal('metadataConfidence')),
    contributor_id: currentUser.id,
    validation_status: 'pending',
    status: 'pending'
  };

  Object.keys(payload).forEach(k => {
    if (payload[k] === '' || payload[k] === undefined) payload[k] = null;
  });

  try {
    let error;
    if (pendingContributeTerm?.isNewTerm) {
      const termSuggestionPayload = {
        proposed_term_name: getVal('newTermName'),
        proposed_term_language: getVal('newTermLanguage'),
        proposed_synonyms: splitSemiColon(getVal('newTermSynonyms')),
        related_existing_term: getVal('newTermRelatedExisting'),
        reason_for_suggestion: getVal('newTermReason'),
        first_definition: payload,
        contributor_id: currentUser.id,
        status: 'pending_review'
      };
      Object.keys(termSuggestionPayload).forEach(k => {
        if (termSuggestionPayload[k] === '' || termSuggestionPayload[k] === undefined) termSuggestionPayload[k] = null;
      });
      ({ error } = await supa.from('term_suggestions').insert(termSuggestionPayload));
    } else {
      ({ error } = await supa.from('definitions').insert(payload));
    }
    if (error) throw error;
    showMsg(msg, pendingContributeTerm?.isNewTerm ? '✓ Thank you. Your new term and first definition have been submitted for review.' : '✓ Thank you. Your definition has been submitted for source review.', 'success');
    btn.textContent = 'Submitted ✓';
    setTimeout(() => { closeContrib(); loadTerms(); }, 1200);
  } catch (e) {
    console.error('Submission error:', e);
    showMsg(msg, 'Could not submit. This usually means the Supabase table is missing one of the new metadata columns/tables, or RLS does not allow this insert. Error: ' + (e.message || e), 'error');
    btn.disabled = false;
    btn.textContent = 'Submit definition for review';
  }
}


const ANNO_I18N = {
  en: {
    title:'Annotate this existing definition', subtitle:'Add missing context, provenance, tags, and verification notes. You do not need to repeat information that is already visible unless you want to correct or enrich it.',
    note:'Please write your annotation in the same language as the definition where possible. You may use English if you are correcting metadata or adding a cross-language note.', preview:'Definition being annotated', section1:'1. Source check and missing source metadata',
    sourceCheck:'Source verification', sourceTerm:'Source term label / synonym, if different', sourceType:'Source type', locatorType:'Locator type', exactLocator:'Exact locator', fullCitation:'Full citation or correction', section2:'2. Definition provenance', provenance:'How does the source present this definition?', provenanceHint:'Only list earlier sources if they are actually cited by the source you checked.', addEarlier:'+ Add earlier cited source', section3:'3. Conceptual annotation', disciplines:'Discipline(s)', researchContexts:'Research context(s)', style:'Definition style', scope:'Definition scope', tags:'Tags', comment:'Why might someone choose, avoid, or contextualise this definition?', confidence:'Annotation confidence', status:'Suggested source-check status', submit:'Submit annotation', statusHelp:'<strong>Note:</strong> Source-verified only means that the wording and metadata appear to match the cited source. It does not mean this is a recommended, preferred, or conceptually superior definition.', sourceTermPlaceholder:'e.g. Scientific impact instead of Academic impact', locatorPlaceholder:'e.g. p. 35; Section 2.1; Slide 12', citationPlaceholder:'Provide missing or corrected citation details if needed.', otherSpecify:'If Other, please specify', commentPlaceholder:'e.g. Useful for teaching, but too narrow for interdisciplinary work.', select:'Select...', sourceMatch:'I checked — definition matches the cited source', sourcePartial:'I checked — partly matches / needs attention', sourceNoMatch:'I checked — does NOT match', sourceNoAccess:'I could not access the source', sourceNotChecked:'I did not check the source'
  },
  de: {
    title:'Diese bestehende Definition annotieren', subtitle:'Ergänzen Sie fehlenden Kontext, Provenienz, Tags und Hinweise zur Quellenprüfung. Bereits sichtbare Informationen müssen nicht wiederholt werden, außer Sie möchten sie korrigieren oder ergänzen.',
    note:'Bitte verfassen Sie Ihre Annotation nach Möglichkeit auf Deutsch, da die ausgewählte Definition auf Deutsch ist. Für quersprachliche Hinweise oder Metadatenkorrekturen können Sie auch Englisch verwenden.', preview:'Zu annotierende Definition', section1:'1. Quellenprüfung und fehlende Quellenmetadaten',
    sourceCheck:'Quellenprüfung', sourceTerm:'In der Quelle verwendetes Term-Label / Synonym, falls abweichend', sourceType:'Quellentyp', locatorType:'Fundstellentyp', exactLocator:'Genaue Fundstelle', fullCitation:'Vollständige Zitation oder Korrektur', section2:'2. Provenienz der Definition', provenance:'Wie stellt die Quelle diese Definition dar?', provenanceHint:'Listen Sie frühere Quellen nur auf, wenn sie tatsächlich in der von Ihnen geprüften Quelle zitiert werden.', addEarlier:'+ Frühere zitierte Quelle hinzufügen', section3:'3. Konzeptuelle Annotation', disciplines:'Disziplin(en)', researchContexts:'Forschungskontext(e)', style:'Definitionsstil', scope:'Umfang der Definition', tags:'Tags', comment:'Warum könnte jemand diese Definition wählen, vermeiden oder kontextualisieren?', confidence:'Sicherheit der Annotation', status:'Vorgeschlagener Quellenprüfungsstatus', submit:'Annotation einreichen', statusHelp:'<strong>Hinweis:</strong> Quellengeprüft bedeutet nur, dass Wortlaut und Metadaten zur zitierten Quelle zu passen scheinen. Es bedeutet nicht, dass diese Definition empfohlen, bevorzugt oder konzeptuell überlegen ist.', sourceTermPlaceholder:'z. B. „wissenschaftliche Wirkung“ statt „akademische Wirkung“', locatorPlaceholder:'z. B. S. 35; Abschnitt 2.1; Folie 12', citationPlaceholder:'Fehlende oder korrigierte Zitationsangaben hier eintragen.', otherSpecify:'Falls „Andere“, bitte angeben', commentPlaceholder:'z. B. nützlich für die Lehre, aber zu eng für interdisziplinäre Arbeiten.', select:'Auswählen...', sourceMatch:'Geprüft — Definition stimmt mit der zitierten Quelle überein', sourcePartial:'Geprüft — stimmt teilweise überein / benötigt Prüfung', sourceNoMatch:'Geprüft — stimmt NICHT überein', sourceNoAccess:'Ich konnte auf die Quelle nicht zugreifen', sourceNotChecked:'Ich habe die Quelle nicht geprüft'
  },
  zh: {
    title:'註釋這條現有定義', subtitle:'補充缺失的語境、來源脈絡、標籤和來源核查說明。除非需要更正或補充，否則不必重複已顯示的資料。',
    note:'請盡量使用與該定義相同的語言撰寫註釋。若是跨語言說明或修正書目資料，也可以使用英文。', preview:'正在註釋的定義', section1:'1. 來源核查與缺失的來源資料',
    sourceCheck:'來源核查', sourceTerm:'來源中使用的術語標籤／同義詞（如有不同）', sourceType:'來源類型', locatorType:'定位類型', exactLocator:'確切位置', fullCitation:'完整引用或修正', section2:'2. 定義來源脈絡', provenance:'該來源如何呈現這一定義？', provenanceHint:'只有在你核查的來源實際引用了早期來源時，才列出這些早期來源。', addEarlier:'+ 新增早期被引用來源', section3:'3. 概念註釋', disciplines:'學科', researchContexts:'研究語境', style:'定義類型／風格', scope:'定義範圍', tags:'標籤', comment:'為什麼有人會選用、避免或需要特別說明這一定義？', confidence:'註釋信心', status:'建議來源核查狀態', submit:'提交註釋', statusHelp:'<strong>注意：</strong>來源已核查只表示文字和資料看起來與引用來源相符，並不表示這是被推薦、較佳或概念上更優越的定義。', sourceTermPlaceholder:'例如：以「科學影響」表示「學術影響」', locatorPlaceholder:'例如：第35頁；第2.1節；第12張投影片', citationPlaceholder:'如有需要，請提供缺失或修正後的引用資料。', otherSpecify:'如選「其他」，請說明', commentPlaceholder:'例如：適合教學使用，但對跨學科討論而言可能過窄。', select:'請選擇...', sourceMatch:'我已核查 — 定義與引用來源相符', sourcePartial:'我已核查 — 部分相符／需要注意', sourceNoMatch:'我已核查 — 與來源不符', sourceNoAccess:'我無法取得該來源', sourceNotChecked:'我沒有核查來源'
  }
};
ANNO_I18N['zh-trad'] = ANNO_I18N.zh;
ANNO_I18N['zh-hant'] = ANNO_I18N.zh;
ANNO_I18N['zh-cn'] = ANNO_I18N.zh;
function annoLangKey(lang) {
  const x = String(lang || 'en').toLowerCase();
  if (x.startsWith('de')) return 'de';
  if (x.startsWith('zh')) return 'zh';
  return 'en';
}
function setAnnoText(id, value, html=false) {
  const el = document.getElementById(id);
  if (!el) return;
  if (html) el.innerHTML = value;
  else el.textContent = value;
}
function localizeAnnotationForm(lang) {
  const t = ANNO_I18N[annoLangKey(lang)] || ANNO_I18N.en;
  setAnnoText('annoTitle', t.title); setAnnoText('annoSubtitle', t.subtitle); setAnnoText('annoLanguageNote', t.note);
  setAnnoText('annoPreviewTitle', t.preview); setAnnoText('annoSection1', t.section1); setAnnoText('annoSourceCheckLabel', t.sourceCheck);
  setAnnoText('annoSourceTermLabelLabel', t.sourceTerm); setAnnoText('annoSourceTypeLabel', t.sourceType); setAnnoText('annoLocatorTypeLabel', t.locatorType);
  setAnnoText('annoExactLocatorLabel', t.exactLocator); setAnnoText('annoFullCitationLabel', t.fullCitation); setAnnoText('annoSection2', t.section2);
  setAnnoText('annoProvenanceLabel', t.provenance); setAnnoText('annoProvenanceHint', t.provenanceHint); setAnnoText('annoAddEarlierSourceBtn', t.addEarlier);
  setAnnoText('annoSection3', t.section3); setAnnoText('annoDisciplineLabel', t.disciplines); setAnnoText('annoResearchContextLabel', t.researchContexts);
  setAnnoText('annoDefinitionStyleLabel', t.style); setAnnoText('annoDefinitionScopeLabel', t.scope); setAnnoText('annoTagsLabel', t.tags);
  setAnnoText('annoCommentLabel', t.comment); setAnnoText('annoConfidenceLabel', t.confidence); setAnnoText('annoSuggestedStatusLabel', t.status);
  setAnnoText('annoStatusHelp', t.statusHelp, true); setAnnoText('btnSubmitAnno', t.submit);
  document.querySelectorAll('[data-i18n-placeholder]').forEach(el => { const key = el.getAttribute('data-i18n-placeholder'); if (t[key]) el.placeholder = t[key]; });
  document.querySelectorAll('#annoSourceCheck option[data-i18n-key]').forEach(opt => { const key = opt.getAttribute('data-i18n-key'); if (t[key]) opt.textContent = t[key]; });
}

function openAnnotate(event, defId) {
  if (event) event.stopPropagation();
  if (!currentUser) { openModal('login'); return; }
  const def = latestPanelDefinitions.find(d => String(d.id) === String(defId)) || {};
  localizeAnnotationForm(def.language || 'en');
  document.getElementById('contribModeSubmit').style.display = 'none';
  document.getElementById('contribModeAnnotate').style.display = 'block';
  document.getElementById('annoDefinitionId').value = defId || '';
  const preview = document.getElementById('annoDefinitionPreview');
  if (preview) {
    const lang = escHtml(def.language || 'Unknown language');
    const source = escHtml(def.source_type || 'Unknown source type');
    const authorYear = escHtml((def.citation_author || '') + (def.citation_year ? ', ' + def.citation_year : ''));
    const locator = escHtml(def.source_location || 'No locator stored yet');
    const sourceLabelText = escHtml(def.source_term_label || 'Same as selected term / not specified');
    const text = escHtml(def.definition_text || 'Definition text could not be loaded. Please close and reopen this panel.');
    preview.innerHTML = '<div class="annotated-definition-meta">'
      + '<span><strong>Language:</strong> ' + lang + '</span>'
      + '<span><strong>Source type:</strong> ' + source + '</span>'
      + (authorYear.trim() ? '<span><strong>Citation:</strong> ' + authorYear + '</span>' : '')
      + '<span><strong>Source label:</strong> ' + sourceLabelText + '</span>'
      + '<span><strong>Locator:</strong> ' + locator + '</span>'
      + '</div><div class="annotated-definition-text">' + text + '</div>';
  }
  const msg = document.getElementById('annoMsg');
  if (msg) msg.innerHTML = '';
  document.getElementById('contribBackdrop').style.display = 'flex';
  document.getElementById('contribBackdrop').classList.add('open');
  document.body.classList.add('modal-open');
}

function addAnnotationOriginalSource() {
  const container = document.getElementById('annoOriginalSourcesContainer');
  if (!container) return;
  const div = document.createElement('div');
  div.className = 'original-source-item';
  div.innerHTML = `
    <button type="button" class="remove-row-btn" onclick="removeOriginalSource(this)">Remove</button>
    <div style="font-size:.75rem;font-weight:700;color:var(--green-dark);margin-bottom:.55rem;">Earlier cited source</div>
    <div class="form-grid-2">
      <div class="form-group"><label class="form-label">Original source author(s)</label><input class="form-input orig-author" type="text" placeholder="Surname, Initials.; Surname, Initials." /></div>
      <div class="form-group"><label class="form-label">Original source year</label><input class="form-input orig-year" type="number" placeholder="2014" min="1500" max="2100" /></div>
    </div>
    <div class="form-group"><label class="form-label">Original source title</label><input class="form-input orig-title" type="text" /></div>
    <div class="form-grid-3"><div class="form-group"><label class="form-label">Original DOI</label><input class="form-input orig-doi" type="text" /></div><div class="form-group"><label class="form-label">Original ISBN</label><input class="form-input orig-isbn" type="text" /></div><div class="form-group"><label class="form-label">Original URL</label><input class="form-input orig-url" type="url" /></div></div>
    <label class="check-row"><input type="checkbox" class="orig-verified" /> I personally checked this original cited source.</label>
  `;
  container.appendChild(div);
}

function collectAnnotationOriginalSources() {
  const container = document.getElementById('annoOriginalSourcesContainer');
  if (!container) return [];
  return Array.from(container.querySelectorAll('.original-source-item')).map(item => {
    const q = cls => (item.querySelector(cls)?.value || '').trim();
    return {
      authors: q('.orig-author'),
      year: q('.orig-year') ? Number(q('.orig-year')) : null,
      title: q('.orig-title'),
      doi: q('.orig-doi').replace(/^https?:\/\/(dx\.)?doi\.org\//i, ''),
      isbn: q('.orig-isbn'),
      url: q('.orig-url'),
      verified_by_annotator: !!item.querySelector('.orig-verified')?.checked
    };
  }).filter(src => src.authors || src.year || src.title || src.doi || src.isbn || src.url);
}

async function submitAnnotation() {
  const msg = document.getElementById('annoMsg');
  const btn = document.getElementById('btnSubmitAnno');
  if (!currentUser) { openModal('login'); return; }
  const defId = getVal('annoDefinitionId');
  if (!defId) { showMsg(msg, 'Missing definition ID.', 'error'); return; }
  if (!getVal('annoConfidence')) { showMsg(msg, 'Please select your annotation confidence.', 'error'); return; }
  btn.disabled = true; btn.textContent = 'Submitting…';
  const payload = {
    definition_id: defId,
    contributor_id: currentUser.id,   // stored as contributor_id for consistent leaderboard queries
    source_check: getVal('annoSourceCheck'),
    source_term_label: getVal('annoSourceTermLabel'),
    source_type: getVal('annoSourceType'),
    source_location_type: getVal('annoSourceLocationType'),
    source_location: getVal('annoSourceLocation'),
    full_citation: getVal('annoFullCitation'),
    definition_provenance: getVal('annoDefinitionProvenance'),
    original_sources: collectAnnotationOriginalSources(),
    disciplines: getCheckedValues('annoDisciplineTags'),
    discipline_other: getVal('annoDisciplineOther'),
    research_contexts: getCheckedValues('annoResearchContextTags'),
    research_context_other: getVal('annoResearchContextOther'),
    definition_style: getVal('annoDefinitionStyle'),
    definition_scope: getVal('annoDefinitionScope'),
    tags: getCheckedValues('annoTags'),
    annotation_note: getVal('annoComment'),   // stored as annotation_note (matches leaderboard query)
    comment: getVal('annoComment'),           // also stored as comment for backward compatibility
    confidence: Number(getVal('annoConfidence')),
    suggested_status: getVal('annoSuggestedStatus')
  };
  Object.keys(payload).forEach(k => { if (payload[k] === '') payload[k] = null; });
  try {
    const { error } = await supa.from('definition_annotations').insert(payload);
    if (error) throw error;
    showMsg(msg, '✓ Thank you. Your annotation has been submitted.', 'success');
    btn.textContent = 'Submitted ✓';
    setTimeout(() => { closeContrib(); }, 1200);
  } catch(e) {
    console.error('Annotation error:', e);
    showMsg(msg, 'Could not submit annotation. You may need to create the definition_annotations table or update RLS. Error: ' + (e.message || e), 'error');
    btn.disabled = false; btn.textContent = 'Submit annotation';
  }
}

/* v9 hard-code cleanup: force all visible submission/annotation labels, hints and option text
   to the selected form language. Stored values remain unchanged. */
const FORM_UI_TEXT_V9 = {
  en: {
    sourceLocationLabel: 'Where can another person find this definition?', exactLocatorLabel: 'Exact page / section / slide / entry',
    sourceInfo: '2. SOURCE INFORMATION', sourceType: 'Source type', fullCitation: 'Full citation', currentAuthors: 'Author(s) of the current source', year: 'Year', currentTitle: 'Title of current source', outlet: 'Publication outlet / publisher / organisation', accessDate: 'Access date (for websites/slides)', sourceAccessibility: 'Source accessibility',
    contribTitle: 'Contribute a definition', contribSubtitle: 'Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.',
    sourceInfoHint: 'Journal articles usually have a DOI. Books usually have an ISBN. Slides may only have presenter, institution, year, and slide number.',
    authorHint: 'Preferred format: Surname, Initials.; separate multiple authors with semicolons. If unsure, paste the author list as shown by the source.'
  },
  de: {
    sourceLocationLabel: 'Wo kann eine andere Person diese Definition finden?', exactLocatorLabel: 'Exakte Seite / Abschnitt / Folie / Eintrag',
    sourceInfo: '2. QUELLENINFORMATIONEN', sourceType: 'Quellentyp', fullCitation: 'Vollständige Quellenangabe', currentAuthors: 'Autor:innen der aktuellen Quelle', year: 'Jahr', currentTitle: 'Titel der aktuellen Quelle', outlet: 'Zeitschrift / Verlag / Organisation', accessDate: 'Zugriffsdatum (für Websites/Folien)', sourceAccessibility: 'Zugänglichkeit der Quelle',
    contribTitle: 'Definition beitragen', contribSubtitle: 'Reichen Sie eine Definition aus einer überprüfbaren Quelle ein. Bitte bewahren Sie den Wortlaut der Quelle und übernehmen Sie Zitationen, die innerhalb der Definition erscheinen.',
    sourceInfoHint: 'Zeitschriftenartikel haben häufig eine DOI. Bücher haben häufig eine ISBN. Folien haben möglicherweise nur Vortragende, Institution, Jahr und Foliennummer.',
    authorHint: 'Bevorzugtes Format: Nachname, Initialen; mehrere Autor:innen mit Semikolon trennen. Wenn unsicher, übernehmen Sie die Autor:innenliste so, wie sie in der Quelle erscheint.'
  },
  'zh-hans': {
    sourceLocationLabel: '其他人可以在哪里找到这个定义？', exactLocatorLabel: '精确页码 / 章节 / 幻灯片 / 词条',
    sourceInfo: '2. 来源信息', sourceType: '来源类型', fullCitation: '完整引用', currentAuthors: '当前来源的作者', year: '年份', currentTitle: '当前来源标题', outlet: '期刊 / 出版社 / 机构', accessDate: '访问日期（网站/幻灯片）', sourceAccessibility: '来源可获取性',
    contribTitle: '提交定义', contribSubtitle: '请提交来自可核查来源的定义。请保留来源中的原文措辞，并保留定义内部出现的引用。',
    sourceInfoHint: '期刊论文通常有 DOI，书籍通常有 ISBN。幻灯片可能只有主讲人、机构、年份和幻灯片编号。',
    authorHint: '如果来源本身是中文出版物，请直接填写中文作者全名（姓氏+名字），如：陈大文；李小明。如果来源是外文出版物，请使用外文参考文献格式，如：Smith, J.; Wang, X.。'
  },
  'zh-hant': {
    sourceLocationLabel: '其他人可以在哪裡找到這個定義？', exactLocatorLabel: '精確頁碼 / 章節 / 投影片 / 詞條',
    sourceInfo: '2. 來源資訊', sourceType: '來源類型', fullCitation: '完整引用', currentAuthors: '當前來源的作者', year: '年份', currentTitle: '當前來源標題', outlet: '期刊 / 出版社 / 機構', accessDate: '存取日期（網站/投影片）', sourceAccessibility: '來源可取得性',
    contribTitle: '提交定義', contribSubtitle: '請提交來自可核查來源的定義。請保留來源中的原文措辭，並保留定義內部出現的引用。',
    sourceInfoHint: '期刊論文通常有 DOI，書籍通常有 ISBN。投影片可能只有主講人、機構、年份和投影片編號。',
    authorHint: '如果來源本身是中文出版物，請直接填寫中文作者全名（姓氏+名字），如：陳大文；李小明。如果來源是外文出版物，請使用外文參考文獻格式，如：Smith, J.; Chan, Y.。'
  }
};
function setLabelForFieldV9(fieldId, text, required=false){
  const el=document.getElementById(fieldId); if(!el) return;
  const lab=el.closest('.form-group')?.querySelector('.form-label');
  if(lab) lab.innerHTML = text + (required ? ' <span style="color:#c0392b">*</span>' : '');
}
function applyHardCodedFormLanguageV9(lang){
  const key=fullLangKey(lang); const t=FORM_UI_TEXT_V9[key]||FORM_UI_TEXT_V9.en;
  const title=document.getElementById('contribTitle'); if(title) title.textContent=t.contribTitle;
  const sub=document.getElementById('contribSubtitle'); if(sub) sub.textContent=t.contribSubtitle;
  setLabelForFieldV9('sourceLocationType', t.sourceLocationLabel, true);
  setLabelForFieldV9('sourceLocation', t.exactLocatorLabel, true);
  setLabelForFieldV9('contribSourceType', t.sourceType, true);
  setLabelForFieldV9('fullCitation', t.fullCitation, true);
  setLabelForFieldV9('citAuthor', t.currentAuthors, true);
  setLabelForFieldV9('citYear', t.year, true);
  setLabelForFieldV9('citTitle', t.currentTitle, true);
  setLabelForFieldV9('publicationOutlet', t.outlet, false);
  setLabelForFieldV9('accessDate', t.accessDate, false);
  setLabelForFieldV9('sourceAccessibility', t.sourceAccessibility, true);
  const hint=document.getElementById('sourceTypeHint'); if(hint) hint.textContent=t.sourceInfoHint;
  const authorHint=document.getElementById('authorFormatHint'); if(authorHint) authorHint.textContent=t.authorHint;
  document.querySelectorAll('.form-section-title').forEach(s=>{
    const txt=s.textContent.trim();
    if(txt==='2. SOURCE INFORMATION'||txt==='2. QUELLENINFORMATIONEN'||txt==='2. 来源信息'||txt==='2. 來源資訊') s.textContent=t.sourceInfo;
  });
}
const _oldFullLocalizeSubmissionFormV9 = window.fullLocalizeSubmissionForm;
window.fullLocalizeSubmissionForm = function(){
  if(typeof _oldFullLocalizeSubmissionFormV9 === 'function') _oldFullLocalizeSubmissionFormV9();
  const lang=(document.getElementById('contribLang')||{}).value||'en';
  applyHardCodedFormLanguageV9(lang);
};
const _oldUpdateContribLanguageHelpersV9 = window.updateContribLanguageHelpers;
window.updateContribLanguageHelpers = function(){
  if(typeof _oldUpdateContribLanguageHelpersV9 === 'function') _oldUpdateContribLanguageHelpersV9();
  const lang=(document.getElementById('contribLang')||{}).value||'en';
  applyHardCodedFormLanguageV9(lang);
};

/* ─────────────────────────────────────────────────────────────
   v10 complete language-specific form UI layer
   Important: displayed labels are localized; stored values remain canonical keys
   (e.g., psychology, possible_jingle_fallacy_same_label_different_meaning).
   This keeps cross-linguistic analyses consistent.
   ───────────────────────────────────────────────────────────── */
(function(){
  function k(lang){
    const x=String(lang||'en').toLowerCase();
    if(x.includes('hant')||x.includes('trad')) return 'zh-hant';
    if(x.startsWith('zh')) return 'zh-hans';
    if(x.startsWith('de')) return 'de';
    return 'en';
  }
  const I={
    en:{
      select:'Select...', skip:'Skip', check:'Check', submitDef:'Submit definition for review', submitAnno:'Submit annotation', remove:'Remove',
      principle:'<strong>Submission principle:</strong> Re-SearchTerms collects source-based definitions, not contributor-made definitions. DOI/URL is useful when available, but books, textbooks, dictionaries, slides, teaching materials, and other sources are welcome if you provide a full citation and a precise page, section, slide, or entry location.',
      title:'Contribute a definition', subtitle:'Submit a definition from a verifiable source. Please preserve the source wording and include citations that appear inside the definition.',
      sections:['1. Definition information','2. Source information','3. Definition provenance','4. Conceptual context and annotation','5. Contributor confidence','6. Verification checklist'],
      annoSections:['1. Source check and missing source metadata','2. Definition provenance','3. Conceptual annotation'],
      labels:{contribTerm:'Term being defined',sourceTermLabel:'Term label or synonym used by the source',contribLang:'Language of submitted definition',definitionType:'Definition wording type',originalSourceWording:'Original source wording',contribText:'Definition text',sourceLocationType:'Where can another person find this definition?',sourceLocation:'Exact page / section / slide / entry',contribSourceType:'Source type',fullCitation:'Full citation',citAuthor:'Author(s) of the current source',citYear:'Year',citTitle:'Title of current source',publicationOutlet:'Publication outlet / publisher / organisation',accessDate:'Access date (for websites/slides)',citDoi:'DOI',citationIsbn:'ISBN',citUrl:'URL',sourceAccessibility:'Source accessibility',definitionProvenance:'How does the current source present this definition?',disciplineOther:'If you selected “Other”, please specify',researchContextOther:'If you selected “Other”, please specify',definitionStyle:'Definition framing / style',definitionScope:'Scope of definition',contributorNote:'Why might someone choose this definition?',selfConfidence:'Confidence that the definition was copied/translated accurately',metadataConfidence:'Confidence that the source metadata are correct',
      annoSourceCheck:'Source verification',annoSourceTermLabel:'Source term label / synonym, if different',annoSourceType:'Source type',annoSourceLocationType:'Locator type',annoExactLocator:'Exact locator',annoFullCitation:'Full citation or correction',annoDefinitionProvenance:'How does the source present this definition?',annoDefinitionStyle:'Definition framing / style',annoDefinitionScope:'Scope of definition',annoComment:'Why might someone choose, avoid, or contextualise this definition?',annoConfidence:'Annotation confidence',annoSuggestedStatus:'Suggested source-check status',annoDisciplineOther:'If you selected “Other”, please specify',annoResearchContextOther:'If you selected “Other”, please specify'},
      headings:{disciplines:'Discipline(s)',contexts:'Research context(s)',tags:'Community tags',suitable:'Suitable context(s)',annoTags:'Tags'},
      hints:{sourceTermLabel:'Use this to capture jingle–jangle cases: the source may define the same or a closely related concept using a different label.',definitionType:'Own synthesis or paraphrase is not allowed. If you translate it yourself, you must also provide the original source wording below.',originalSourceWording:'Required if you submit your own translation. This helps bilingual validators compare your translation with the source text.',definitionText:'<strong>Important:</strong> include citations that appear inside the definition, for example “Open science refers to … (Fecher & Friesike, 2014).” Do not remove parenthetical citations because they are useful for provenance and future citation-network analyses.',sourceLocationType:'This is not the source type. It tells validators where inside the source the definition appears.',sourceType:'Journal articles usually have a DOI. Books usually have an ISBN. Slides may only have presenter, institution, year, and slide number.',author:'Preferred format: Surname, Initials.; separate multiple authors with semicolons. If unsure, paste the author list as shown by the source.',style:'A full contributor handbook can explain these categories; this short guide is included to reduce uncertainty.',provenance:'Only list earlier sources that are cited by the current source in relation to the definition. Do not add sources merely because you personally think they are relevant.',annoStatus:'<strong>Note:</strong> Source-verified only means that the wording and metadata appear to match the cited source. It does not mean this is a recommended, preferred, or conceptually superior definition.'},
      ph:{sourceTermLabel:'e.g. scientific impact, if the selected database term is academic impact',originalSourceWording:'Paste the definition in the original language as it appears in the source.',contribText:'Paste the definition. Keep in-text citations such as (Smith, 2014) if they appear in the source definition.',sourceLocation:'e.g. p. 35; Section 2.1; Slide 18; Entry: Open Science',fullCitation:'Paste or type the full citation as completely as possible.',citAuthor:'Surname, Initials.; Surname, Initials. e.g. Smith, J.; Jones, A. B.',citYear:'2024',citTitle:'e.g. Open Science: A Practical Guide',publicationOutlet:'Journal, publisher, institution, website, organisation...',citDoi:'10.xxxx/xxxxx',citationIsbn:'For books/textbooks',citUrl:'https://...',disciplineOther:'If Other, please specify',researchContextOther:'If Other, please specify',contributorNote:'e.g. Useful when discussing transparency in quantitative psychology, but perhaps too narrow for interdisciplinary work.',annoSourceTermLabel:'e.g. Scientific impact instead of Academic impact',annoExactLocator:'e.g. p. 35; Section 2.1; Slide 12',annoFullCitation:'Provide missing or corrected citation details if needed.',annoComment:'e.g. Useful for teaching, but too narrow for interdisciplinary work.',annoDisciplineOther:'If Other, please specify',annoResearchContextOther:'If Other, please specify'},
      options:{
        contribLang:{en:'English',de:'German','zh-hans':'Chinese Simplified','zh-hans':'Chinese (简/繁)',other:'Other'},
        definitionType:{exact_source_wording:'Exact wording as it appears in the source',source_provided_translation:'Translation provided by the source itself',contributor_translation_of_source:'Contributor translation of a source definition'},
        sourceLocationType:{'':'Select...',page:'Page number(s)',section:'Named section',chapter:'Chapter',slide:'Slide number',glossary_entry:'Glossary entry',dictionary_entry:'Dictionary entry',appendix:'Appendix',paragraph:'Paragraph number',table_or_figure:'Table or figure',other:'Other locator'},
        sourceType:{journal_article:'Journal article',book:'Book',book_chapter:'Book chapter',textbook:'Textbook',dictionary:'Dictionary',glossary:'Glossary',website:'Website',report:'Report',policy_document:'Policy document',conference_paper:'Conference paper',slides:'Lecture / workshop slides',teaching_material:'Teaching material',other:'Other'},
        sourceAccessibility:{'':'Select...',open_access:'Open access',institutional_access:'Institutional access required',physical_copy:'Physical copy only',personal_copy:'Personal copy',unknown:'Unknown'},
        provenance:{'':'Select...',original_to_current_source:'The current source appears to propose this definition itself',direct_quote_from_cited_source:'The current source directly quotes an earlier cited source',adapted_from_cited_source:'The current source adapts or paraphrases an earlier cited source',synthesises_multiple_cited_sources:'The current source combines or summarises multiple earlier cited sources',provenance_unclear:'Unclear / no explicit provenance given'},
        style:{'':'Not sure / skip',theoretical:'Theoretical — explains what the concept means',operational:'Operational — specifies how the concept is measured or identified',normative:'Normative — states what should be done or valued',descriptive:'Descriptive — describes common usage or features',procedural:'Procedural — describes steps, practices, or processes',educational:'Educational — written for teaching or learning',policy_oriented:'Policy-oriented — written for governance, regulation, or institutional use',other:'Other'},
        scope:{'':'Not sure / skip',very_broad:'Very broad',broad:'Broad',moderate:'Moderate',narrow:'Narrow',very_narrow:'Very narrow'},
        confidence:{'':'Select...',5:'5 — Very confident',4:'4 — Confident',3:'3 — Moderately confident',2:'2 — Somewhat uncertain',1:'1 — Uncertain'},
        sourceCheck:{'':'Select...',definition_matches_source:'I checked — definition matches the cited source',definition_partly_matches_source:'I checked — partly matches / needs attention',definition_does_not_match_source:'I checked — does NOT match',source_not_accessible:'I could not access the source',not_checked:'I did not check the source'},
        status:{'':'No status suggestion',source_verified:'Source-verified: definition matches cited source',flagged:'Flagged for review',archived:'Archive / duplicate / superseded'}
      },
      tags:{psychology:'Psychology',medicine:'Medicine',education:'Education',linguistics:'Linguistics',neuroscience:'Neuroscience',philosophy:'Philosophy',statistics:'Statistics',computer_science:'Computer science',engineering:'Engineering',library_information_science:'Library & information science',sociology:'Sociology',political_science:'Political science',economics_business:'Economics / Business',communication_media:'Communication / Media studies',law:'Law / Legal studies',humanities:'Humanities',environmental_science:'Environmental science',social_science:'Social sciences',interdisciplinary:'Interdisciplinary',other:'Other',open_science:'Open science',research_methods:'Research methods',measurement:'Measurement',theory_development:'Theory development',research_design:'Research design',meta_research:'Meta-research',scientific_communication:'Scientific communication',research_ethics:'Research ethics',clinical_practice:'Clinical practice',policy:'Policy',technology_innovation:'Technology / Innovation',impact_assessment:'Impact assessment',knowledge_translation:'Knowledge translation',industry:'Industry',academic_research:'Academic research',teaching:'Teaching',student_learning:'Student learning',grant_writing:'Grant writing',public_communication:'Public communication','discipline-specific':'Discipline-specific',contested:'Contested','emerging-concept':'Emerging concept',historical:'Historical',outdated:'Outdated',ambiguous:'Ambiguous','jargon-heavy':'Jargon-heavy','accessible-to-non-experts':'Accessible to non-experts','overly-broad':'Overly broad','overly-narrow':'Overly narrow','frequently-used':'Frequently used','rarely-used':'Rarely used',normative:'Normative','value-laden':'Value-laden',possible_jingle_fallacy_same_label_different_meaning:'Possible jingle fallacy: same label, different meaning',possible_jangle_fallacy_different_label_similar_meaning:'Possible jangle fallacy: different label, similar meaning',term_used_differently_from_other_definitions:'Term used differently from other definitions',different_concept_under_same_term:'May refer to a different concept'},
      checks:['I copied the definition exactly as it appears in the source, except where I explicitly marked it as a translation.','I kept the in-text citations that appear inside the definition, if any.','I confirm this is not my own synthesis, interpretation, or paraphrase.','I checked that the citation metadata are as accurate as possible.','I provided a page, section, slide, entry, or other locator so another person can verify the definition.','I understand that the submission may be reviewed, source-verified, flagged, or archived by project contributors.']
    }
  };
  I.de=JSON.parse(JSON.stringify(I.en)); Object.assign(I.de,{select:'Auswählen...',skip:'Überspringen',check:'Prüfen',submitDef:'Definition zur Prüfung einreichen',submitAnno:'Annotation einreichen',remove:'Entfernen',title:'Definition beitragen',subtitle:'Reichen Sie eine Definition aus einer überprüfbaren Quelle ein. Bitte bewahren Sie den Wortlaut der Quelle und übernehmen Sie Zitationen, die innerhalb der Definition erscheinen.',principle:'<strong>Einreichungsprinzip:</strong> Re-SearchTerms sammelt quellengestützte Definitionen, keine von Beitragenden selbst erstellten Definitionen. DOI/URL sind nützlich, wenn vorhanden; Bücher, Lehrbücher, Wörterbücher, Folien, Lehrmaterialien und andere Quellen sind willkommen, sofern eine vollständige Zitation und eine genaue Seite, Abschnitts-, Folien- oder Eintragsangabe angegeben werden.',sections:['1. Definitionsangaben','2. Quellenangaben','3. Provenienz der Definition','4. Konzeptueller Kontext und Annotation','5. Sicherheit der Beitragenden','6. Prüfliste']});
  Object.assign(I.de.labels,{contribTerm:'Definierter Begriff',sourceTermLabel:'In der Quelle verwendetes Begriffslabel oder Synonym',contribLang:'Sprache der eingereichten Definition',definitionType:'Art des Definitionswortlauts',originalSourceWording:'Originalwortlaut der Quelle',contribText:'Definitionstext',sourceLocationType:'Wo kann eine andere Person diese Definition finden?',sourceLocation:'Exakte Seite / Abschnitt / Folie / Eintrag',contribSourceType:'Quellentyp',fullCitation:'Vollständige Zitation',citAuthor:'Autor:innen der aktuellen Quelle',citYear:'Jahr',citTitle:'Titel der aktuellen Quelle',publicationOutlet:'Zeitschrift / Verlag / Organisation',accessDate:'Zugriffsdatum (für Websites/Folien)',sourceAccessibility:'Zugänglichkeit der Quelle',definitionProvenance:'Wie stellt die aktuelle Quelle diese Definition dar?',disciplineOther:'Falls „Andere“, bitte angeben',researchContextOther:'Falls „Andere“, bitte angeben',definitionStyle:'Definitionsrahmung / Stil',definitionScope:'Umfang der Definition',contributorNote:'Warum könnte jemand diese Definition wählen?',selfConfidence:'Sicherheit, dass die Definition korrekt kopiert/übersetzt wurde',metadataConfidence:'Sicherheit, dass die Quellenmetadaten korrekt sind',annoSourceCheck:'Quellenprüfung',annoSourceTermLabel:'Begriffslabel / Synonym in der Quelle, falls abweichend',annoSourceType:'Quellentyp',annoSourceLocationType:'Fundstellentyp',annoExactLocator:'Genaue Fundstelle',annoFullCitation:'Vollständige Zitation oder Korrektur',annoDefinitionProvenance:'Wie stellt die Quelle diese Definition dar?',annoDefinitionStyle:'Definitionsrahmung / Stil',annoDefinitionScope:'Umfang der Definition',annoComment:'Warum könnte jemand diese Definition wählen, vermeiden oder kontextualisieren?',annoConfidence:'Sicherheit der Annotation',annoSuggestedStatus:'Vorgeschlagener Quellenprüfungsstatus'});
  Object.assign(I.de.headings,{disciplines:'Disziplin(en)',contexts:'Forschungskontext(e)',tags:'Community-Tags',suitable:'Geeignete Kontexte',annoTags:'Tags'});
  Object.assign(I.de.hints,{sourceTermLabel:'Damit können Jingle-Jangle-Fälle erfasst werden: Die Quelle kann dasselbe oder ein nah verwandtes Konzept unter einem anderen Label definieren.',definitionType:'Eigene Synthesen oder Paraphrasen sind nicht erlaubt. Wenn Sie selbst übersetzen, müssen Sie unten auch den Originalwortlaut der Quelle angeben.',originalSourceWording:'Erforderlich, wenn Sie eine eigene Übersetzung einreichen. So können zweisprachige Prüfende die Übersetzung mit dem Quellentext vergleichen.',definitionText:'<strong>Wichtig:</strong> Übernehmen Sie Zitationen, die innerhalb der Definition erscheinen, z. B. „Open Science bezeichnet … (Fecher & Friesike, 2014).“ Entfernen Sie Klammerzitationen nicht, da sie für Provenienz- und Zitationsnetzwerkanalysen nützlich sind.',sourceLocationType:'Dies ist nicht der Quellentyp. Es gibt an, wo innerhalb der Quelle die Definition zu finden ist.',sourceType:'Zeitschriftenartikel haben häufig eine DOI. Bücher haben häufig eine ISBN. Folien enthalten möglicherweise nur Vortragende, Institution, Jahr und Foliennummer.',author:'Bevorzugtes Format: Nachname, Initialen.; mehrere Autor:innen mit Semikolon trennen. Wenn unsicher, übernehmen Sie die Autor:innenliste so, wie sie in der Quelle erscheint.',style:'Ein Beitragendenhandbuch kann diese Kategorien ausführlicher erklären; die Kurzbeschreibung soll Unsicherheit reduzieren.',provenance:'Listen Sie nur frühere Quellen auf, die in der aktuellen Quelle im Zusammenhang mit dieser Definition tatsächlich zitiert werden.',annoStatus:'<strong>Hinweis:</strong> Quellengeprüft bedeutet nur, dass Wortlaut und Metadaten zur zitierten Quelle zu passen scheinen. Es bedeutet nicht, dass diese Definition empfohlen, bevorzugt oder konzeptuell überlegen ist.'});
  Object.assign(I.de.ph,{sourceTermLabel:'z. B. wissenschaftliche Wirkung, wenn der Datenbankbegriff akademische Wirkung ist',originalSourceWording:'Fügen Sie den Originalwortlaut der Definition aus der Quelle ein.',contribText:'Fügen Sie die Definition ein. Behalten Sie Zitationen wie (Smith, 2014) bei, wenn sie in der Quelle erscheinen.',sourceLocation:'z. B. S. 35; Abschnitt 2.1; Folie 18; Eintrag: Open Science',fullCitation:'Fügen Sie die vollständige Quellenangabe so vollständig wie möglich ein.',citAuthor:'Nachname, Initialen.; Nachname, Initialen. z. B. Smith, J.; Müller, A.',citTitle:'z. B. Open Science: Ein praktischer Leitfaden',publicationOutlet:'Zeitschrift, Verlag, Institution, Website, Organisation ...',citationIsbn:'Für Bücher/Lehrbücher',disciplineOther:'Falls Andere, bitte angeben',researchContextOther:'Falls Andere, bitte angeben',contributorNote:'z. B. nützlich für Diskussionen zu Transparenz in der quantitativen Psychologie, aber für interdisziplinäre Arbeiten vielleicht zu eng.',annoSourceTermLabel:'z. B. wissenschaftliche Wirkung statt akademische Wirkung',annoExactLocator:'z. B. S. 35; Abschnitt 2.1; Folie 12',annoFullCitation:'Fehlende oder korrigierte Zitationsangaben hier eintragen.',annoComment:'z. B. nützlich für die Lehre, aber zu eng für interdisziplinäre Arbeiten.',annoDisciplineOther:'Falls Andere, bitte angeben',annoResearchContextOther:'Falls Andere, bitte angeben'});
  Object.assign(I.de.options.contribLang,{en:'Englisch',de:'Deutsch','zh-hans':'Chinesisch (vereinfacht)','zh-hant':'Chinesisch (traditionell)',other:'Andere'}); Object.assign(I.de.options.definitionType,{exact_source_wording:'Exakter Wortlaut aus der Quelle',source_provided_translation:'Von der Quelle selbst bereitgestellte Übersetzung',contributor_translation_of_source:'Übersetzung einer Quelldefinition durch Beitragende'}); Object.assign(I.de.options.sourceLocationType,{'':'Auswählen...',page:'Seitenzahl(en)',section:'Benannter Abschnitt',chapter:'Kapitel',slide:'Foliennummer',glossary_entry:'Glossareintrag',dictionary_entry:'Wörterbucheintrag',appendix:'Anhang',paragraph:'Absatznummer',table_or_figure:'Tabelle oder Abbildung',other:'Andere Fundstelle'}); Object.assign(I.de.options.sourceType,{journal_article:'Zeitschriftenartikel',book:'Buch',book_chapter:'Buchkapitel',textbook:'Lehrbuch',dictionary:'Wörterbuch',glossary:'Glossar',website:'Website',report:'Bericht',policy_document:'Policy-Dokument',conference_paper:'Konferenzbeitrag',slides:'Vorlesungs-/Workshop-Folien',teaching_material:'Lehrmaterial',other:'Andere'}); Object.assign(I.de.options.sourceAccessibility,{'':'Auswählen...',open_access:'Open Access',institutional_access:'Institutioneller Zugang erforderlich',physical_copy:'Nur gedrucktes Exemplar',personal_copy:'Persönliches Exemplar',unknown:'Unbekannt'}); Object.assign(I.de.options.provenance,{'':'Auswählen...',original_to_current_source:'Die aktuelle Quelle scheint diese Definition selbst vorzuschlagen',direct_quote_from_cited_source:'Die aktuelle Quelle zitiert eine frühere zitierte Quelle direkt',adapted_from_cited_source:'Die aktuelle Quelle adaptiert oder paraphrasiert eine frühere zitierte Quelle',synthesises_multiple_cited_sources:'Die aktuelle Quelle kombiniert oder fasst mehrere frühere zitierte Quellen zusammen',provenance_unclear:'Unklar / keine explizite Provenienz angegeben'}); Object.assign(I.de.options.style,{'':'Nicht sicher / überspringen',theoretical:'Theoretisch — erklärt, was das Konzept bedeutet',operational:'Operational — legt fest, wie das Konzept gemessen oder identifiziert wird',normative:'Normativ — beschreibt, was getan oder wertgeschätzt werden sollte',descriptive:'Deskriptiv — beschreibt übliche Verwendung oder Merkmale',procedural:'Prozedural — beschreibt Schritte, Praktiken oder Prozesse',educational:'Didaktisch — für Lehre oder Lernen formuliert',policy_oriented:'Policy-orientiert — für Governance, Regulierung oder institutionelle Nutzung formuliert',other:'Andere'}); Object.assign(I.de.options.scope,{'':'Nicht sicher / überspringen',very_broad:'Sehr breit',broad:'Breit',moderate:'Mittel',narrow:'Eng',very_narrow:'Sehr eng'}); Object.assign(I.de.options.confidence,{'':'Auswählen...',5:'5 — Sehr sicher',4:'4 — Sicher',3:'3 — Mäßig sicher',2:'2 — Eher unsicher',1:'1 — Unsicher'}); Object.assign(I.de.options.sourceCheck,{'':'Auswählen...',definition_matches_source:'Geprüft — Definition stimmt mit der zitierten Quelle überein',definition_partly_matches_source:'Geprüft — stimmt teilweise überein / benötigt Aufmerksamkeit',definition_does_not_match_source:'Geprüft — stimmt NICHT überein',source_not_accessible:'Ich konnte auf die Quelle nicht zugreifen',not_checked:'Ich habe die Quelle nicht geprüft'}); Object.assign(I.de.options.status,{'':'Kein Statusvorschlag',source_verified:'Quellengeprüft: Definition stimmt mit zitierter Quelle überein',flagged:'Zur Prüfung markieren',archived:'Archivieren / Duplikat / ersetzt'});
  Object.assign(I.de.tags,{psychology:'Psychologie',medicine:'Medizin',education:'Bildungswissenschaft',linguistics:'Linguistik',neuroscience:'Neurowissenschaft',philosophy:'Philosophie',statistics:'Statistik',computer_science:'Informatik',engineering:'Ingenieurwissenschaften',library_information_science:'Bibliotheks- und Informationswissenschaft',sociology:'Soziologie',political_science:'Politikwissenschaft',economics_business:'Wirtschaftswissenschaften',communication_media:'Kommunikations- / Medienwissenschaft',law:'Rechtswissenschaft',humanities:'Geisteswissenschaften',environmental_science:'Umweltwissenschaften',social_science:'Sozialwissenschaften',interdisciplinary:'Interdisziplinär',other:'Andere',open_science:'Open Science',research_methods:'Forschungsmethoden',measurement:'Messung',theory_development:'Theorieentwicklung',research_design:'Forschungsdesign',meta_research:'Metaforschung',scientific_communication:'Wissenschaftskommunikation',research_ethics:'Forschungsethik',clinical_practice:'Klinische Praxis',policy:'Policy',technology_innovation:'Technologie / Innovation',impact_assessment:'Impact-Bewertung',knowledge_translation:'Wissenstransfer',industry:'Industrie',academic_research:'Wissenschaftliche Forschung',teaching:'Lehre',student_learning:'Studierendenlernen',grant_writing:'Antragstellung',public_communication:'Öffentliche Kommunikation','discipline-specific':'Disziplinspezifisch',contested:'Umstritten','emerging-concept':'Entstehendes Konzept',historical:'Historisch',outdated:'Veraltet',ambiguous:'Mehrdeutig','jargon-heavy':'Jargonlastig','accessible-to-non-experts':'Für Nicht-Expert:innen zugänglich','overly-broad':'Zu breit','overly-narrow':'Zu eng','frequently-used':'Häufig verwendet','rarely-used':'Selten verwendet',normative:'Normativ','value-laden':'Wertgeladen',possible_jingle_fallacy_same_label_different_meaning:'Möglicher Jingle-Fallacy-Fall: gleiches Label, andere Bedeutung',possible_jangle_fallacy_different_label_similar_meaning:'Möglicher Jangle-Fallacy-Fall: anderes Label, ähnliche Bedeutung',term_used_differently_from_other_definitions:'Begriff wird anders verwendet als in anderen Definitionen',different_concept_under_same_term:'Möglicherweise anderes Konzept unter demselben Begriff'});
  I.de.checks=['Ich habe die Definition exakt so übernommen, wie sie in der Quelle erscheint, außer wenn ich sie ausdrücklich als Übersetzung markiert habe.','Ich habe Zitationen innerhalb der Definition beibehalten, sofern vorhanden.','Ich bestätige, dass dies keine eigene Synthese, Interpretation oder Paraphrase ist.','Ich habe geprüft, dass die Zitationsmetadaten so genau wie möglich sind.','Ich habe Seite, Abschnitt, Folie, Eintrag oder eine andere Fundstelle angegeben, damit eine andere Person die Definition überprüfen kann.','Ich verstehe, dass die Einreichung von Projektbeitragenden geprüft, quellengeprüft, markiert oder archiviert werden kann.'];
  // Simplified Chinese
  I['zh-hans']=JSON.parse(JSON.stringify(I.en)); Object.assign(I['zh-hans'],{select:'请选择...',skip:'跳过',check:'检查',submitDef:'提交定义以供审核',submitAnno:'提交注释',remove:'删除',title:'提交定义',subtitle:'请提交来自可核查来源的定义。请保留来源中的原文措辞，并保留定义内部出现的引用。',principle:'<strong>提交原则：</strong>Re-SearchTerms 收集基于来源的定义，而不是贡献者自行创造的定义。如有 DOI/URL 请提供；书籍、教材、词典、幻灯片、教学材料和其他来源也可以提交，但请提供完整引用，以及准确的页码、章节、幻灯片或词条位置。',sections:['1. 定义信息','2. 来源信息','3. 定义来源脉络','4. 概念语境与注释','5. 贡献者信心','6. 核查清单']});
  Object.assign(I['zh-hans'].labels,{contribTerm:'被定义的术语',sourceTermLabel:'来源中使用的术语标签或同义词',contribLang:'提交定义的语言',definitionType:'定义文本类型',originalSourceWording:'来源原文',contribText:'定义文本',sourceLocationType:'其他人可以在哪里找到这个定义？',sourceLocation:'精确页码 / 章节 / 幻灯片 / 词条',contribSourceType:'来源类型',fullCitation:'完整引用',citAuthor:'当前来源的作者',citYear:'年份',citTitle:'当前来源标题',publicationOutlet:'期刊 / 出版社 / 机构',accessDate:'访问日期（网站/幻灯片）',sourceAccessibility:'来源可获取性',definitionProvenance:'当前来源如何呈现这个定义？',disciplineOther:'如选择“其他”，请说明',researchContextOther:'如选择“其他”，请说明',definitionStyle:'定义框架 / 风格',definitionScope:'定义范围',contributorNote:'为什么有人会选用这个定义？',selfConfidence:'对定义复制/翻译准确性的信心',metadataConfidence:'对来源元数据准确性的信心',annoSourceCheck:'来源核查',annoSourceTermLabel:'来源中使用的术语标签 / 同义词（如有不同）',annoSourceType:'来源类型',annoSourceLocationType:'定位类型',annoExactLocator:'确切位置',annoFullCitation:'完整引用或修正',annoDefinitionProvenance:'该来源如何呈现这个定义？',annoDefinitionStyle:'定义框架 / 风格',annoDefinitionScope:'定义范围',annoComment:'为什么有人会选用、避免或需要说明这个定义？',annoConfidence:'注释信心',annoSuggestedStatus:'建议的来源核查状态'});
  Object.assign(I['zh-hans'].headings,{disciplines:'学科',contexts:'研究语境',tags:'社区标签',suitable:'适用语境',annoTags:'标签'});
  Object.assign(I['zh-hans'].hints,{sourceTermLabel:'用于捕捉 jingle/jangle 情况：来源可能用不同标签来定义相同或相近的概念。',definitionType:'不允许自行综合或改写。如果你自行翻译，必须在下方提供来源原文。',originalSourceWording:'如果你提交自己的翻译，则必须填写此项，以便双语核查者比较译文和原文。',definitionText:'<strong>重要：</strong>请保留定义中出现的引用，例如“开放科学指……（Fecher & Friesike, 2014）”。不要删除括号中的引用，因为这些信息有助于来源脉络和未来的引用网络分析。',sourceLocationType:'这不是来源类型，而是告诉核查者定义在来源中的具体位置。',sourceType:'期刊论文通常有 DOI，书籍通常有 ISBN。幻灯片可能只有主讲人、机构、年份和幻灯片编号。',author:'如果来源本身是中文出版物，请直接填写中文作者全名（姓氏+名字），如：陈大文；李小明。如果来源是外文出版物，请使用外文参考文献格式，如：Smith, J.; Wang, X.。',style:'贡献者手册会更详细解释这些类别；这里的简短说明用于减少不确定性。',provenance:'只列出当前来源在定义相关位置实际引用的早期来源；不要仅因为你个人认为相关就添加。',annoStatus:'<strong>注意：</strong>“来源已核查”只表示文字和元数据看起来与引用来源相符，并不表示这是被推荐、较佳或概念上更优越的定义。'});
  Object.assign(I['zh-hans'].ph,{sourceTermLabel:'例如：如果数据库术语是“学术影响”，来源可能使用“科学影响”',originalSourceWording:'请粘贴来源中出现的原文定义。',contribText:'请粘贴定义文本。如来源定义中出现 (Smith, 2014) 这类引用，请保留。',sourceLocation:'例如：第35页；第2.1节；第18张幻灯片；词条：开放科学',fullCitation:'请尽可能完整地粘贴或输入引用信息。',citAuthor:'中文来源请填作者全名，如：陈大文；李小明。外文来源如：Smith, J.; Wang, X.',citTitle:'例如：开放科学实践指南',publicationOutlet:'期刊、出版社、机构、网站、组织……',citationIsbn:'适用于书籍/教材',disciplineOther:'如选择“其他”，请说明',researchContextOther:'如选择“其他”，请说明',contributorNote:'例如：适合讨论定量心理学中的透明性，但对跨学科讨论而言可能过窄。',annoSourceTermLabel:'例如：用“科学影响”表示“学术影响”',annoExactLocator:'例如：第35页；第2.1节；第12张幻灯片',annoFullCitation:'如有需要，请提供缺失或修正后的引用信息。',annoComment:'例如：适合教学使用，但对跨学科讨论而言可能过窄。',annoDisciplineOther:'如选择“其他”，请说明',annoResearchContextOther:'如选择“其他”，请说明'});
  Object.assign(I['zh-hans'].options.contribLang,{en:'英语',de:'德语','zh-hans':'中文简体','zh-hant':'中文繁体',other:'其他'}); Object.assign(I['zh-hans'].options.definitionType,{exact_source_wording:'与来源完全一致的原文措辞',source_provided_translation:'来源本身提供的翻译',contributor_translation_of_source:'贡献者对来源定义的翻译'}); Object.assign(I['zh-hans'].options.sourceLocationType,{'':'请选择...',page:'页码',section:'具名章节',chapter:'章',slide:'幻灯片编号',glossary_entry:'术语表词条',dictionary_entry:'词典词条',appendix:'附录',paragraph:'段落编号',table_or_figure:'表格或图示',other:'其他定位方式'}); Object.assign(I['zh-hans'].options.sourceType,{journal_article:'期刊论文',book:'书籍',book_chapter:'书籍章节',textbook:'教材',dictionary:'词典',glossary:'术语表',website:'网站',report:'报告',policy_document:'政策文件',conference_paper:'会议论文',slides:'讲座 / 工作坊幻灯片',teaching_material:'教学材料',other:'其他'}); Object.assign(I['zh-hans'].options.sourceAccessibility,{'':'请选择...',open_access:'开放获取',institutional_access:'需要机构访问权限',physical_copy:'仅有纸本',personal_copy:'个人副本',unknown:'未知'}); Object.assign(I['zh-hans'].options.provenance,{'':'请选择...',original_to_current_source:'当前来源似乎自行提出这一定义',direct_quote_from_cited_source:'当前来源直接引用了一个较早的被引来源',adapted_from_cited_source:'当前来源改写或改编了一个较早的被引来源',synthesises_multiple_cited_sources:'当前来源综合或概述了多个较早的被引来源',provenance_unclear:'不清楚 / 没有明确说明来源脉络'}); Object.assign(I['zh-hans'].options.style,{'':'不确定 / 跳过',theoretical:'理论性——解释概念是什么意思',operational:'操作性——说明概念如何被测量或识别',normative:'规范性——说明应该怎么做或重视什么',descriptive:'描述性——描述常见用法或特征',procedural:'程序性——描述步骤、实践或过程',educational:'教育性——用于教学或学习',policy_oriented:'政策导向——用于治理、规章或机构实践',other:'其他'}); Object.assign(I['zh-hans'].options.scope,{'':'不确定 / 跳过',very_broad:'非常宽泛',broad:'宽泛',moderate:'中等',narrow:'狭窄',very_narrow:'非常狭窄'}); Object.assign(I['zh-hans'].options.confidence,{'':'请选择...',5:'5 — 非常有信心',4:'4 — 有信心',3:'3 — 中等信心',2:'2 — 有些不确定',1:'1 — 不确定'}); Object.assign(I['zh-hans'].options.sourceCheck,{'':'请选择...',definition_matches_source:'我已核查——定义与引用来源相符',definition_partly_matches_source:'我已核查——部分相符 / 需要注意',definition_does_not_match_source:'我已核查——与来源不符',source_not_accessible:'我无法访问该来源',not_checked:'我没有核查来源'}); Object.assign(I['zh-hans'].options.status,{'':'不建议状态',source_verified:'来源已核查：定义与引用来源相符',flagged:'标记为需要审查',archived:'归档 / 重复 / 已被取代'});
  Object.assign(I['zh-hans'].tags,{psychology:'心理学',medicine:'医学',education:'教育学',linguistics:'语言学',neuroscience:'神经科学',philosophy:'哲学',statistics:'统计学',computer_science:'计算机科学',engineering:'工程学',library_information_science:'图书馆与信息科学',sociology:'社会学',political_science:'政治学',economics_business:'经济学 / 商学',communication_media:'传播 / 媒体研究',law:'法律 / 法学',humanities:'人文学科',environmental_science:'环境科学',social_science:'社会科学',interdisciplinary:'跨学科',other:'其他',open_science:'开放科学',research_methods:'研究方法',measurement:'测量',theory_development:'理论发展',research_design:'研究设计',meta_research:'元研究',scientific_communication:'科学传播',research_ethics:'研究伦理',clinical_practice:'临床实践',policy:'政策',technology_innovation:'技术 / 创新',impact_assessment:'影响评估',knowledge_translation:'知识转化',industry:'产业',academic_research:'学术研究',teaching:'教学',student_learning:'学生学习',grant_writing:'基金申请',public_communication:'公众传播','discipline-specific':'学科特定',contested:'有争议','emerging-concept':'新兴概念',historical:'历史性',outdated:'过时',ambiguous:'含糊','jargon-heavy':'术语密集','accessible-to-non-experts':'非专家也容易理解','overly-broad':'过于宽泛','overly-narrow':'过于狭窄','frequently-used':'常用','rarely-used':'少用',normative:'规范性','value-laden':'带有价值判断',possible_jingle_fallacy_same_label_different_meaning:'可能的 jingle fallacy：同一标签，不同含义',possible_jangle_fallacy_different_label_similar_meaning:'可能的 jangle fallacy：不同标签，相似含义',term_used_differently_from_other_definitions:'该术语的用法不同于其他定义',different_concept_under_same_term:'可能是同一术语下的不同概念'});
  I['zh-hans'].checks=['我已按来源中的原文准确复制定义；如有翻译，已明确标注。','我保留了定义内部出现的文内引用（如有）。','我确认这不是我自己的综合、解释或改写。','我已尽可能核查引用元数据的准确性。','我提供了页码、章节、幻灯片、词条或其他定位信息，以便他人核查。','我理解此提交可能会由项目贡献者审核、来源核查、标记或归档。'];
  // Traditional Chinese derived with explicit labels
  I['zh-hant']=JSON.parse(JSON.stringify(I['zh-hans'])); Object.assign(I['zh-hant'],{select:'請選擇...',skip:'跳過',check:'檢查',submitDef:'提交定義以供審核',submitAnno:'提交註釋',remove:'刪除',title:'提交定義',subtitle:'請提交來自可核查來源的定義。請保留來源中的原文措辭，並保留定義內部出現的引用。',principle:'<strong>提交原則：</strong>Re-SearchTerms 收集基於來源的定義，而不是貢獻者自行創造的定義。如有 DOI/URL 請提供；書籍、教材、詞典、投影片、教學材料和其他來源也可以提交，但請提供完整引用，以及準確的頁碼、章節、投影片或詞條位置。',sections:['1. 定義資訊','2. 來源資訊','3. 定義來源脈絡','4. 概念語境與註釋','5. 貢獻者信心','6. 核查清單']});
  Object.assign(I['zh-hant'].labels,{contribTerm:'被定義的術語',sourceTermLabel:'來源中使用的術語標籤或同義詞',contribLang:'提交定義的語言',definitionType:'定義文本類型',originalSourceWording:'來源原文',contribText:'定義文本',sourceLocationType:'其他人可以在哪裡找到這個定義？',sourceLocation:'精確頁碼 / 章節 / 投影片 / 詞條',contribSourceType:'來源類型',fullCitation:'完整引用',citAuthor:'當前來源的作者',citYear:'年份',citTitle:'當前來源標題',publicationOutlet:'期刊 / 出版社 / 機構',accessDate:'存取日期（網站/投影片）',sourceAccessibility:'來源可取得性',definitionProvenance:'當前來源如何呈現這個定義？',disciplineOther:'如選擇「其他」，請說明',researchContextOther:'如選擇「其他」，請說明',definitionStyle:'定義框架 / 風格',definitionScope:'定義範圍',contributorNote:'為什麼有人會選用這個定義？',selfConfidence:'對定義複製/翻譯準確性的信心',metadataConfidence:'對來源元資料準確性的信心',annoSourceCheck:'來源核查',annoSourceTermLabel:'來源中使用的術語標籤 / 同義詞（如有不同）',annoSourceType:'來源類型',annoSourceLocationType:'定位類型',annoExactLocator:'確切位置',annoFullCitation:'完整引用或修正',annoDefinitionProvenance:'該來源如何呈現這個定義？',annoDefinitionStyle:'定義框架 / 風格',annoDefinitionScope:'定義範圍',annoComment:'為什麼有人會選用、避免或需要說明這個定義？',annoConfidence:'註釋信心',annoSuggestedStatus:'建議的來源核查狀態'});
  Object.assign(I['zh-hant'].headings,{disciplines:'學科',contexts:'研究語境',tags:'社群標籤',suitable:'適用語境',annoTags:'標籤'});
  Object.assign(I['zh-hant'].hints,{sourceTermLabel:'用於捕捉 jingle/jangle 情況：來源可能用不同標籤來定義相同或相近的概念。',definitionType:'不允許自行綜合或改寫。如果你自行翻譯，必須在下方提供來源原文。',originalSourceWording:'如果你提交自己的翻譯，則必須填寫此項，以便雙語核查者比較譯文和原文。',definitionText:'<strong>重要：</strong>請保留定義中出現的引用，例如「開放科學指……（Fecher & Friesike, 2014）」。不要刪除括號中的引用，因為這些資訊有助於來源脈絡和未來的引用網絡分析。',sourceLocationType:'這不是來源類型，而是告訴核查者定義在來源中的具體位置。',sourceType:'期刊論文通常有 DOI，書籍通常有 ISBN。投影片可能只有主講人、機構、年份和投影片編號。',author:'如果來源本身是中文出版物，請直接填寫中文作者全名（姓氏+名字），如：陳大文；李小明。如果來源是外文出版物，請使用外文參考文獻格式，如：Smith, J.; Chan, Y.。',style:'貢獻者手冊會更詳細解釋這些類別；這裡的簡短說明用於減少不確定性。',provenance:'只列出當前來源在定義相關位置實際引用的早期來源；不要僅因為你個人認為相關就添加。',annoStatus:'<strong>注意：</strong>「來源已核查」只表示文字和元資料看起來與引用來源相符，並不表示這是被推薦、較佳或概念上更優越的定義。'});
  Object.assign(I['zh-hant'].ph,{sourceTermLabel:'例如：如果資料庫術語是「學術影響」，來源可能使用「科學影響」',originalSourceWording:'請貼上來源中出現的原文定義。',contribText:'請貼上定義文本。如來源定義中出現 (Smith, 2014) 這類引用，請保留。',sourceLocation:'例如：第35頁；第2.1節；第18張投影片；詞條：開放科學',fullCitation:'請盡可能完整地貼上或輸入引用資訊。',citAuthor:'中文來源請填作者全名，如：陳大文；李小明。外文來源如：Smith, J.; Chan, Y.',citTitle:'例如：開放科學實踐指南',publicationOutlet:'期刊、出版社、機構、網站、組織……',citationIsbn:'適用於書籍/教材',disciplineOther:'如選擇「其他」，請說明',researchContextOther:'如選擇「其他」，請說明',contributorNote:'例如：適合討論定量心理學中的透明性，但對跨學科討論而言可能過窄。',annoSourceTermLabel:'例如：用「科學影響」表示「學術影響」',annoExactLocator:'例如：第35頁；第2.1節；第12張投影片',annoFullCitation:'如有需要，請提供缺失或修正後的引用資訊。',annoComment:'例如：適合教學使用，但對跨學科討論而言可能過窄。',annoDisciplineOther:'如選擇「其他」，請說明',annoResearchContextOther:'如選擇「其他」，請說明'});
  Object.assign(I['zh-hant'].options.contribLang,{en:'英語',de:'德語','zh-hans':'中文簡體','zh-hant':'中文繁體',other:'其他'}); Object.assign(I['zh-hant'].options.sourceLocationType,{'':'請選擇...',page:'頁碼',section:'具名章節',chapter:'章',slide:'投影片編號',glossary_entry:'術語表詞條',dictionary_entry:'詞典詞條',appendix:'附錄',paragraph:'段落編號',table_or_figure:'表格或圖示',other:'其他定位方式'}); Object.assign(I['zh-hant'].options.sourceAccessibility,{'':'請選擇...',open_access:'開放取用',institutional_access:'需要機構存取權限',physical_copy:'僅有紙本',personal_copy:'個人副本',unknown:'未知'}); Object.assign(I['zh-hant'].options.provenance,{'':'請選擇...',original_to_current_source:'當前來源似乎自行提出這一定義',direct_quote_from_cited_source:'當前來源直接引用了一個較早的被引來源',adapted_from_cited_source:'當前來源改寫或改編了一個較早的被引來源',synthesises_multiple_cited_sources:'當前來源綜合或概述了多個較早的被引來源',provenance_unclear:'不清楚 / 沒有明確說明來源脈絡'}); Object.assign(I['zh-hant'].options.style,{'':'不確定 / 跳過',theoretical:'理論性——解釋概念是什麼意思',operational:'操作性——說明概念如何被測量或識別',normative:'規範性——說明應該怎麼做或重視什麼',descriptive:'描述性——描述常見用法或特徵',procedural:'程序性——描述步驟、實踐或過程',educational:'教育性——用於教學或學習',policy_oriented:'政策導向——用於治理、規章或機構實踐',other:'其他'}); Object.assign(I['zh-hant'].options.scope,{'':'不確定 / 跳過',very_broad:'非常寬泛',broad:'寬泛',moderate:'中等',narrow:'狹窄',very_narrow:'非常狹窄'}); Object.assign(I['zh-hant'].options.confidence,{'':'請選擇...',5:'5 — 非常有信心',4:'4 — 有信心',3:'3 — 中等信心',2:'2 — 有些不確定',1:'1 — 不確定'}); Object.assign(I['zh-hant'].options.sourceCheck,{'':'請選擇...',definition_matches_source:'我已核查——定義與引用來源相符',definition_partly_matches_source:'我已核查——部分相符 / 需要注意',definition_does_not_match_source:'我已核查——與來源不符',source_not_accessible:'我無法取得該來源',not_checked:'我沒有核查來源'}); Object.assign(I['zh-hant'].options.status,{'':'不建議狀態',source_verified:'來源已核查：定義與引用來源相符',flagged:'標記為需要審查',archived:'歸檔 / 重複 / 已被取代'});
  Object.assign(I['zh-hant'].tags,{psychology:'心理學',medicine:'醫學',education:'教育學',linguistics:'語言學',neuroscience:'神經科學',philosophy:'哲學',statistics:'統計學',computer_science:'電腦科學',engineering:'工程學',library_information_science:'圖書館與資訊科學',sociology:'社會學',political_science:'政治學',economics_business:'經濟學 / 商學',communication_media:'傳播 / 媒體研究',law:'法律 / 法學',humanities:'人文學科',environmental_science:'環境科學',social_science:'社會科學',interdisciplinary:'跨學科',other:'其他',open_science:'開放科學',research_methods:'研究方法',measurement:'測量',theory_development:'理論發展',research_design:'研究設計',meta_research:'元研究',scientific_communication:'科學傳播',research_ethics:'研究倫理',clinical_practice:'臨床實踐',policy:'政策',technology_innovation:'技術 / 創新',impact_assessment:'影響評估',knowledge_translation:'知識轉化',industry:'產業',academic_research:'學術研究',teaching:'教學',student_learning:'學生學習',grant_writing:'基金申請',public_communication:'公眾傳播','discipline-specific':'學科特定',contested:'有爭議','emerging-concept':'新興概念',historical:'歷史性',outdated:'過時',ambiguous:'含糊','jargon-heavy':'術語密集','accessible-to-non-experts':'非專家也容易理解','overly-broad':'過於寬泛','overly-narrow':'過於狹窄','frequently-used':'常用','rarely-used':'少用',normative:'規範性','value-laden':'帶有價值判斷',possible_jingle_fallacy_same_label_different_meaning:'可能的 jingle fallacy：同一標籤，不同含義',possible_jangle_fallacy_different_label_similar_meaning:'可能的 jangle fallacy：不同標籤，相似含義',term_used_differently_from_other_definitions:'該術語的用法不同於其他定義',different_concept_under_same_term:'可能是同一術語下的不同概念'});
  I['zh-hant'].checks=['我已按來源中的原文準確複製定義；如有翻譯，已明確標註。','我保留了定義內部出現的文內引用（如有）。','我確認這不是我自己的綜合、解釋或改寫。','我已盡可能核查引用元資料的準確性。','我提供了頁碼、章節、投影片、詞條或其他定位資訊，以便他人核查。','我理解此提交可能會由項目貢獻者審核、來源核查、標記或歸檔。'];

  function setOptions(id,map){const el=document.getElementById(id); if(!el||!map) return; const val=el.value; el.innerHTML=''; Object.entries(map).forEach(([v,t])=>{const o=document.createElement('option'); o.value=v; o.textContent=t; el.appendChild(o);}); if([...el.options].some(o=>o.value==val)) el.value=val;}
  function labelFor(id,text,req){const el=document.getElementById(id); if(!el) return; const lab=el.closest('.form-group')?.querySelector('.form-label'); if(lab) lab.innerHTML=text+(req?' <span style="color:#c0392b">*</span>':'');}
  function ph(id,text){const el=document.getElementById(id); if(el&&text) el.placeholder=text;}
  function hint(id,html){const el=document.getElementById(id); if(el&&html) el.innerHTML=html;}
  function tags(id,map){const el=document.getElementById(id); if(!el) return; el.querySelectorAll('label').forEach(l=>{const inp=l.querySelector('input'); if(inp && map[inp.value]){ while(l.childNodes.length>1) l.removeChild(l.lastChild); l.appendChild(document.createTextNode(' '+map[inp.value])); }});}
  function sectionTitles(containerSelector,titles){const c=document.querySelector(containerSelector); if(!c) return; c.querySelectorAll('.form-section-title').forEach((el,i)=>{if(titles[i]) el.textContent=titles[i];});}
  function localizeAll(lang){const T=I[k(lang)]; if(!T) return; const modeSubmit=document.getElementById('contribModeSubmit'); const modeAnno=document.getElementById('contribModeAnnotate');
    const ct=document.getElementById('contribTitle'); if(ct) ct.textContent=T.title; const cs=document.getElementById('contribSubtitle'); if(cs) cs.textContent=T.subtitle; const hb=modeSubmit?.querySelector('.helper-box'); if(hb) hb.innerHTML=T.principle;
    sectionTitles('#contribModeSubmit',T.sections); sectionTitles('#contribModeAnnotate',T.annoSections);
    ['contribTerm','sourceTermLabel','contribLang','definitionType','originalSourceWording','contribText','sourceLocationType','sourceLocation','contribSourceType','fullCitation','citAuthor','citYear','citTitle','publicationOutlet','accessDate','citDoi','citationIsbn','citUrl','sourceAccessibility','definitionProvenance','disciplineOther','researchContextOther','definitionStyle','definitionScope','contributorNote','selfConfidence','metadataConfidence','annoSourceCheck','annoSourceTermLabel','annoSourceType','annoSourceLocationType','annoExactLocator','annoFullCitation','annoDefinitionProvenance','annoDefinitionStyle','annoDefinitionScope','annoComment','annoConfidence','annoSuggestedStatus','annoDisciplineOther','annoResearchContextOther'].forEach(id=>labelFor(id,T.labels[id]||'', ['contribTerm','contribLang','definitionType','contribText','sourceLocationType','sourceLocation','contribSourceType','fullCitation','citAuthor','citYear','citTitle','sourceAccessibility','definitionProvenance','selfConfidence','metadataConfidence'].includes(id)));
    const headMap={annoDisciplineLabel:T.headings.disciplines,annoResearchContextLabel:T.headings.contexts}; Object.entries(headMap).forEach(([id,txt])=>{const el=document.getElementById(id); if(el) el.textContent=txt;});
    document.querySelectorAll('#contribModeSubmit .form-label').forEach(el=>{if(el.textContent.trim()==='Community tags'||el.textContent.trim()==='社群標籤'||el.textContent.trim()==='社区标签'||el.textContent.trim()==='Community-Tags') el.textContent=T.headings.tags; if(el.textContent.trim()==='Suitable context(s)'||el.textContent.trim()==='適用語境'||el.textContent.trim()==='适用语境'||el.textContent.trim()==='Geeignete Kontexte') el.textContent=T.headings.suitable;});
    ['sourceTermLabel','originalSourceWording','contribText','sourceLocation','fullCitation','citAuthor','citYear','citTitle','publicationOutlet','citDoi','citationIsbn','citUrl','disciplineOther','researchContextOther','contributorNote','annoSourceTermLabel','annoExactLocator','annoFullCitation','annoComment','annoDisciplineOther','annoResearchContextOther'].forEach(id=>ph(id,T.ph[id]));
    hint('sourceTermLabelHint',T.hints.sourceTermLabel); hint('definitionTypeHint',T.hints.definitionType); hint('originalSourceWordingHint',T.hints.originalSourceWording); hint('definitionTextHelp',T.hints.definitionText); hint('sourceLocationTypeHint',T.hints.sourceLocationType); hint('sourceTypeHint',T.hints.sourceType); hint('authorFormatHint',T.hints.author); hint('annoStatusHelp',T.hints.annoStatus); document.querySelectorAll('#contribModeSubmit .form-hint').forEach(el=>{if(el.textContent.includes('contributor handbook')||el.textContent.includes('Beitragendenhandbuch')||el.textContent.includes('贡献者手册')||el.textContent.includes('貢獻者手冊')) el.textContent=T.hints.style;});
    setOptions('contribLang',T.options.contribLang); setOptions('definitionType',T.options.definitionType); setOptions('sourceLocationType',T.options.sourceLocationType); setOptions('contribSourceType',T.options.sourceType); setOptions('sourceAccessibility',T.options.sourceAccessibility); setOptions('definitionProvenance',T.options.provenance); setOptions('definitionStyle',T.options.style); setOptions('definitionScope',T.options.scope); setOptions('selfConfidence',T.options.confidence); setOptions('metadataConfidence',T.options.confidence); setOptions('annoSourceCheck',T.options.sourceCheck); setOptions('annoSourceType',T.options.sourceType); setOptions('annoSourceLocationType',T.options.sourceLocationType); setOptions('annoDefinitionProvenance',T.options.provenance); setOptions('annoDefinitionStyle',T.options.style); setOptions('annoDefinitionScope',T.options.scope); setOptions('annoConfidence',T.options.confidence); setOptions('annoSuggestedStatus',T.options.status);
    tags('disciplineTags',T.tags); tags('researchContextTags',T.tags); tags('selfTags',T.tags); tags('suitableContextTags',T.tags); tags('annoDisciplineTags',T.tags); tags('annoResearchContextTags',T.tags); tags('annoTags',T.tags);
    document.querySelectorAll('#contribModeSubmit .check-row').forEach((row,i)=>{ if(T.checks[i]){const inp=row.querySelector('input'); row.innerHTML=''; if(inp) row.appendChild(inp); row.appendChild(document.createTextNode(' '+T.checks[i])); }});
    const btn=document.getElementById('btnSubmitContrib'); if(btn) btn.textContent=T.submitDef; const abtn=document.getElementById('btnSubmitAnno'); if(abtn) abtn.textContent=T.submitAnno; const chk=document.getElementById('btnVerifyDoi'); if(chk) chk.textContent=T.check;
  }
  const oldUH=window.updateContribLanguageHelpers; window.updateContribLanguageHelpers=function(){ if(typeof oldUH==='function') oldUH.apply(this,arguments); setTimeout(()=>localizeAll(document.getElementById('contribLang')?.value||'en'),0); };
  const oldOpen=window.openContrib; window.openContrib=function(){ const r=oldOpen.apply(this,arguments); setTimeout(()=>localizeAll(document.getElementById('contribLang')?.value||'en'),0); return r; };
  const oldAnno=window.openAnnotate; window.openAnnotate=function(event,defId){ const r=oldAnno.apply(this,arguments); setTimeout(()=>{const def=(window.latestPanelDefinitions||latestPanelDefinitions||[]).find(d=>String(d.id)===String(defId))||{}; localizeAll(def.language||'en');},0); return r; };
  window.deepLocalizeReSearchTermsFormV10=localizeAll;
  document.addEventListener('change',e=>{ if(e.target && e.target.id==='contribLang') localizeAll(e.target.value); });
})();

/* ─────────────────────────────────────────────────────────────
   Static Definition-Level Network 2.0
   Legacy JSON + live Supabase multilingual definitions/annotations
   ───────────────────────────────────────────────────────────── */
let legacyDefinitionNodes = [];
let legacyDefinitionEdges = [];
let definitionNetwork = null;
let definitionNetworkLoaded = false;
let currentDefinitionGraphItems = new Map();
let currentDefinitionEdges = new Map();
let liveDefinitionsForCurrentTerm = [];

function defNormText(x) {
  return String(x || '').toLowerCase().replace(/\s+/g, ' ').replace(/[“”"'.,;:!?()\[\]]/g, '').trim().slice(0, 180);
}
function defLangNorm(lang) {
  const x = String(lang || 'en').toLowerCase();
  if (x.includes('hant') || x.includes('traditional')) return 'zh-hant';
  if (x.includes('hans') || x.includes('simplified')) return 'zh-hans';
  if (x === 'zh' || x.startsWith('zh')) return 'zh';
  if (x.startsWith('de')) return 'de';
  return 'en';
}
function defSourceBucket(row) {
  const s = String(row.source || row.source_type || row.citation_title || '').toLowerCase();
  if (s.includes('forrt') || s.includes('glossary_seed')) return 'forrt';
  if (s.includes('wiktionary')) return 'wiktionary';
  if (s.includes('igi') || s.includes('publisher_dictionary')) return 'igi';
  if (row.__live && row.contributor_id) return 'community';
  return 'other';
}
function defShape(row) {
  const b = defSourceBucket(row);
  if (b === 'forrt') return 'star';
  if (b === 'wiktionary') return 'triangle';
  if (b === 'igi') return 'square';
  if (b === 'community') return 'diamond';
  return 'dot';
}
function defSourceLabel(row) {
  if (row.source) return row.source;
  if (row.source_type === 'glossary_seed') return 'FORRT / glossary seed';
  if (row.source_type === 'wiktionary_api') return 'Wiktionary';
  if (row.source_type === 'publisher_dictionary') return 'IGI InfoSci-Dictionary';
  return row.source_type || row.citation_title || 'Unknown source';
}
function defStatus(row) {
  const verified = Number(row.source_verified_count || 0);
  const mismatch = Number(row.source_mismatch_count || 0);
  const annotations = Array.isArray(row.annotations) ? row.annotations.length : 0;
  if (mismatch > 0) return 'flagged';
  if (verified > 0) return 'checked';
  if (annotations > 0) return 'annotated';
  return 'unchecked';
}
function defNodeStyle(row) {
  const status = defStatus(row);
  // Border encodes verification status
  let border = '#999', borderWidth = 1;
  if (status === 'checked') { border = '#1a7a4a'; borderWidth = 4; }
  if (status === 'flagged') { border = '#d88700'; borderWidth = 4; }
  if (status === 'annotated') { border = '#4a7c5f'; borderWidth = 3; }
  // Background uses cluster colour (matching term-level clustering graph)
  const clusterName = row.cluster_name || row.__term?.cluster_name || null;
  let bg;
  if (clusterName) {
    // Use same palette as termClusterColor but lighter (alpha tint)
    const palette = ['#3d6b59','#7a9e7e','#d88700','#9b5de5','#00a6a6','#ef476f','#577590','#bc6c25','#6d597a','#2a9d8f'];
    let h = 0; String(clusterName).split('').forEach(ch => h = (h * 31 + ch.charCodeAt(0)) >>> 0);
    const hex = palette[h % palette.length];
    // Convert to light tint: append 40 for ~25% opacity as hex
    bg = hex + '55';
    if (!border || border === '#999') border = hex;
  } else {
    // Fallback to source-based colour
    const source = defSourceBucket(row);
    bg = '#d6e4dc';
    if (source === 'forrt') bg = '#a8d4bc';
    if (source === 'wiktionary') bg = '#f2d58a';
    if (source === 'igi') bg = '#cbd7e8';
    if (source === 'community') bg = '#e5c4f2';
  }
  return { bg, border, borderWidth };
}
function defPreview(x, n = 110) {
  x = String(x || '').replace(/\s+/g, ' ').trim();
  return x.length > n ? x.slice(0, n) + '…' : x;
}
function escapeHtml(x) {
  return String(x ?? '').replace(/[&<>'"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;',"'":'&#39;','"':'&quot;'}[c]));
}
function stripHtml(x) {
  const tmp = document.createElement('div');
  tmp.innerHTML = String(x || '');
  return (tmp.textContent || tmp.innerText || '').replace(/\s+/g, ' ').trim();
}
function plainTooltip(parts) {
  return parts.filter(Boolean).map(x => stripHtml(x)).join('\n');
}
function normaliseOrcid(orcid) {
  const raw = String(orcid || '').trim();
  if (!raw) return '';
  return raw.replace(/^https?:\/\/(www\.)?orcid\.org\//i, '').replace(/^orcid:?\s*/i, '').trim();
}
function contributorInfo(row) {
  const name = row.contributor_display_name || row.contributor_name || row.display_name || row.full_name || row.profiles?.display_name || row.profiles?.full_name || '';
  const orcid = normaliseOrcid(row.contributor_orcid || row.orcid || row.profiles?.orcid || '');
  const id = row.contributor_id || '';
  return { name, orcid, id };
}
function contributorHtml(row) {
  const c = contributorInfo(row);
  if (c.name) {
    const orcid = c.orcid ? ` <a href="https://orcid.org/${escapeHtml(c.orcid)}" target="_blank" rel="noopener" style="color:var(--green-mid);text-decoration:underline;">ORCID ↗</a>` : '';
    return `${escapeHtml(c.name)}${orcid}`;
  }
  if (c.id) return `Contributor ID: ${escapeHtml(c.id)}`;
  return 'Seed/imported definition';
}
async function enrichDefinitionsWithContributorProfiles(defs) {
  const ids = [...new Set((defs || []).map(d => d.contributor_id).filter(Boolean))];
  if (!ids.length || !supa) return defs || [];
  let profiles = [];
  // Try common profile column names. This is safe: if the table/view or column does not exist, we silently fall back to contributor_id.
  try {
    const r = await supa.from('profiles').select('id, user_id, display_name, full_name, orcid').in('id', ids);
    if (!r.error && Array.isArray(r.data)) profiles = profiles.concat(r.data);
  } catch(e) {}
  try {
    const r = await supa.from('profiles').select('id, user_id, display_name, full_name, orcid').in('user_id', ids);
    if (!r.error && Array.isArray(r.data)) profiles = profiles.concat(r.data);
  } catch(e) {}
  try {
    const r = await supa.from('v_contributor_profiles').select('id, user_id, display_name, full_name, orcid').in('user_id', ids);
    if (!r.error && Array.isArray(r.data)) profiles = profiles.concat(r.data);
  } catch(e) {}
  const byId = new Map();
  profiles.forEach(p => {
    if (p.id) byId.set(String(p.id), p);
    if (p.user_id) byId.set(String(p.user_id), p);
  });
  return (defs || []).map(d => {
    const p = byId.get(String(d.contributor_id || ''));
    return p ? {
      ...d,
      contributor_display_name: p.display_name || p.full_name || d.contributor_display_name,
      contributor_orcid: p.orcid || d.contributor_orcid
    } : d;
  });
}

async function initDefinitionNetworkPage() {
  if (definitionNetworkLoaded) return;
  definitionNetworkLoaded = true;
  const graph = document.getElementById('definitionGraph');
  if (graph) graph.innerHTML = '<div class="definition-loading">Loading static definition network data…</div>';
  try {
    const [nodesResp, edgesResp] = await Promise.all([
      fetch('data/definition_nodes.json'),
      fetch('data/definition_edges.json')
    ]);
    legacyDefinitionNodes = await nodesResp.json();
    legacyDefinitionEdges = await edgesResp.json();
    populateDefinitionTermSelect();
    const route = location.hash.replace(/^#/, '').split('?');
    const selectedSlug = route[0] === 'definitions' ? new URLSearchParams(route[1] || '').get('term') : '';
    if (selectedSlug) {
      const t = termFromKey(selectedSlug);
      if (t?.name) document.getElementById('definitionTermSelect').value = t.name;
    }
    renderDefinitionNetwork();
  } catch (e) {
    console.error('Definition network load failed', e);
    if (graph) graph.innerHTML = '<div class="definition-loading">Could not load definition network JSON. Make sure <code>data/definition_nodes.json</code> and <code>data/definition_edges.json</code> are uploaded next to this HTML file.</div>';
  }
}

function populateDefinitionTermSelect() {
  const sel = document.getElementById('definitionTermSelect');
  if (!sel) return;
  const legacyTerms = [...new Set(legacyDefinitionNodes.map(d => String(d.concept || '').trim()).filter(Boolean))];
  const supaTerms = (window.allTerms || allTerms || []).map(t => t.name_en).filter(Boolean);
  const terms = [...new Set([...supaTerms, ...legacyTerms])].sort((a,b)=>a.localeCompare(b));
  sel.innerHTML = '<option value="">Select a term…</option>' + terms.map(t => `<option value="${escapeHtml(t)}">${escapeHtml(t)}</option>`).join('');
}

function findSupabaseTermByName(termName) {
  const lower = String(termName || '').toLowerCase();
  return (window.allTerms || allTerms || []).find(t =>
    String(t.name_en || '').toLowerCase() === lower ||
    String(t.name_de || '').toLowerCase() === lower ||
    String(t.name_zh || '').toLowerCase() === lower ||
    String(t.slug || '').toLowerCase() === lower.replace(/\s+/g, '_')
  );
}

async function fetchLiveDefinitionsForTerm(termName) {
  liveDefinitionsForCurrentTerm = [];
  if (!supa) return [];
  const term = findSupabaseTermByName(termName);
  if (!term || !term.id) return [];
  try {
    const { data, error } = await supa
      .from('v_definitions_public')
      .select('*')
      .eq('term_id', term.id)
      .order('language', { ascending: true });
    if (error) throw error;
    const enriched = await enrichDefinitionsWithContributorProfiles(data || []);
    liveDefinitionsForCurrentTerm = enriched.map(d => ({...d, __live:true, __term:term}));
    return liveDefinitionsForCurrentTerm;
  } catch(e) {
    console.warn('Could not fetch live definitions for network', e);
    return [];
  }
}

function passLanguageFilter(row, selectedLang) {
  if (selectedLang === 'all') return true;
  const lang = defLangNorm(row.language || (row.__legacy ? 'en' : ''));
  if (selectedLang === 'zh') return lang.startsWith('zh');
  return lang === selectedLang;
}
function passSourceFilter(row, selectedSource) {
  if (selectedSource === 'all') return true;
  return defSourceBucket(row) === selectedSource;
}

async function renderDefinitionNetwork() {
  const termName = document.getElementById('definitionTermSelect')?.value || '';
  const langFilter = document.getElementById('definitionLangSelect')?.value || 'all';
  const sourceFilter = document.getElementById('definitionSourceSelect')?.value || 'all';
  const minSim = Number(document.getElementById('definitionMinSim')?.value || 0);
  const graph = document.getElementById('definitionGraph');
  if (!graph) return;
  if (!termName) {
    graph.innerHTML = '<div class="definition-loading">Choose a term to load the definition network.</div>';
    document.getElementById('definitionDetailPanel').innerHTML = '<h3>Selected definition</h3><p class="definition-network-note">Click a node to inspect definition metadata.</p>';
    return;
  }
  graph.innerHTML = '<div class="definition-loading">Loading network…</div>';

  const legacyRowsAll = legacyDefinitionNodes
    .filter(d => String(d.concept || '').toLowerCase() === termName.toLowerCase())
    .map(d => ({...d, id:'legacy:'+d.def_ID, language:'en', __legacy:true}));
  const liveRowsRaw = await fetchLiveDefinitionsForTerm(termName);

  // Deduplicate imported live English definitions that are already represented in legacy data.
  const legacyTextSet = new Set(legacyRowsAll.map(d => defNormText(d.definition)).filter(Boolean));
  const liveRows = liveRowsRaw
    .filter(d => {
      if (d.contributor_id) return true;
      if (defLangNorm(d.language) !== 'en') return true;
      return !legacyTextSet.has(defNormText(d.definition_text));
    })
    .map(d => ({...d, id:'live:'+d.id, definition:d.definition_text, source:defSourceLabel(d)}));

  let rows = [...legacyRowsAll, ...liveRows].filter(d => passLanguageFilter(d, langFilter) && passSourceFilter(d, sourceFilter));
  const allowedIds = new Set(rows.map(d => d.id));
  const legacyIdMap = new Map(legacyRowsAll.map(d => [d.def_ID, d.id]));

  const nodes = rows.map(d => {
    const style = defNodeStyle(d);
    const annotations = Number(d.source_verified_count || 0) + Number(d.source_mismatch_count || 0) + (Array.isArray(d.annotations) ? d.annotations.length : 0);
    const star = annotations > 0 ? '⭐ ' : '';
    // Build label: use term's native name for non-English live definitions
    const langCode = defLangNorm(d.language || 'en');
    // Get the term's name in the definition's language
    const termObj = (window.allTerms || allTerms || []).find(t =>
      t.name_en && String(t.name_en).toLowerCase() === String(termName).toLowerCase()
    );
    let nativeName = '';
    if (langCode === 'de' && termObj?.name_de) nativeName = termObj.name_de;
    else if ((langCode === 'zh' || langCode === 'zh-hans' || langCode === 'zh-hant') && termObj?.name_zh) nativeName = termObj.name_zh;
    // Short source ID for disambiguation when multiple defs in same language
    const shortId = d.id ? String(d.id).slice(-4) : '';
    const liveLabel = nativeName
      ? `[${langCode.toUpperCase()}] ${nativeName}${shortId ? '-'+shortId : ''}`
      : (d.source_term_label || `[${langCode.toUpperCase()}] ${shortId}`);
    const label = `${star}${d.__legacy ? d.def_ID : (langCode === 'en' ? (d.source_term_label || d.def_ID || shortId) : liveLabel)}`;
    return {
      id: d.id,
      label,
      title: plainTooltip([
        `${d.source_term_label || d.term || d.__term?.name_en || d.concept || 'Definition'}`,
        `Language: ${defLangNorm(d.language || 'en').toUpperCase()} · ${defSourceLabel(d)}`,
        (d.cluster_name || d.__term?.cluster_name) ? `Cluster: ${d.cluster_name || d.__term?.cluster_name}` : '',
        'Click for full details'
      ].filter(Boolean)),
      shape: defShape(d),
      size: d.__live && d.contributor_id ? 26 : (defSourceBucket(d)==='forrt' ? 28 : 22),
      color: { background: style.bg, border: style.border, highlight: { background: '#f6e6a6', border: '#d88700' } },
      borderWidth: style.borderWidth,
      font: { size: 14 }
    };
  });

  const edges = legacyDefinitionEdges
    .filter(e => String(e.concept || '').toLowerCase() === termName.toLowerCase())
    .filter(e => Number(e.cosine_similarity) >= minSim)
    .map(e => {
      const from = legacyIdMap.get(e.def_ID1);
      const to = legacyIdMap.get(e.def_ID2);
      if (!from || !to || !allowedIds.has(from) || !allowedIds.has(to)) return null;
      const w = Number(e.cosine_similarity || 0);
      return {
        id: `edge:${e.def_ID1}___${e.def_ID2}`,
        from, to,
        value: Math.max(1, w * 10),
        width: Math.max(1, 1 + w * 8),
        color: { color: '#9aa8a0', highlight: '#d88700' },
        title: `Cosine similarity: ${w.toFixed(3)}\nClick to compare definitions`,
        __data: e
      };
    }).filter(Boolean);

  currentDefinitionGraphItems = new Map(rows.map(r => [r.id, r]));
  currentDefinitionEdges = new Map(edges.map(e => [e.id, e]));

  if (nodes.length === 0) {
    graph.innerHTML = '<div class="definition-loading">No definitions found for this filter. Try “All languages” or “All sources”.</div>';
    return;
  }

  const data = { nodes: new vis.DataSet(nodes), edges: new vis.DataSet(edges) };
  const options = {
    interaction: { hover: true, tooltipDelay: 80, navigationButtons: true, keyboard: true },
    physics: { enabled: true, solver: 'forceAtlas2Based', stabilization: { iterations: 180 } },
    nodes: { shadow: false },
    edges: { smooth: { type: 'dynamic' }, scaling: { min: 1, max: 12 } }
  };
  graph.innerHTML = '';
  definitionNetwork = new vis.Network(graph, data, options);
  definitionNetwork.on('click', params => {
    if (params.nodes && params.nodes.length) showDefinitionNodeDetails(params.nodes[0]);
    else if (params.edges && params.edges.length) showDefinitionEdgeDetails(params.edges[0]);
  });

  const detail = document.getElementById('definitionDetailPanel');
  detail.innerHTML = `<h3>${escapeHtml(termName)}</h3>
    <div class="definition-detail-meta">
      <span class="definition-pill">${nodes.length} nodes</span>
      <span class="definition-pill">${edges.length} similarity edges</span>
      <span class="definition-pill">${liveRows.length} live Supabase nodes</span>
    </div>
    <p class="definition-network-note">Nodes are coloured by term-level cluster (consistent across all analysis tabs). Edges represent SBERT semantic similarity. Click any node to see full definition and source details, or click an edge to compare two definitions side by side.`;
}

function showDefinitionNodeDetails(nodeId) {
  const row = currentDefinitionGraphItems.get(nodeId);
  const detail = document.getElementById('definitionDetailPanel');
  if (!row || !detail) return;
  const sourceCheck = Number(row.source_verified_count || 0);
  const mismatch = Number(row.source_mismatch_count || 0);
  const noAccess = Number(row.source_no_access_count || 0);
  const annotations = Array.isArray(row.annotations) ? row.annotations : [];
  const statusPill = mismatch > 0 ? '<span class="definition-pill bad">Flagged/source mismatch</span>' :
    sourceCheck > 0 ? '<span class="definition-pill good">Source checked</span>' :
    annotations.length > 0 ? '<span class="definition-pill good">Annotated</span>' :
    '<span class="definition-pill">Not yet source-checked</span>';
  const canAnnotate = row.__live && String(row.id || '').startsWith('live:');
  const liveId = canAnnotate ? String(row.id).replace('live:', '') : '';
  const link = row.citation_url || row.hyperlink || row.source_work_url || '';
  detail.innerHTML = `<h3>${escapeHtml(row.source_term_label || row.term || row.__term?.name_en || row.concept || 'Definition')}</h3>
    <div class="definition-detail-meta">
      <span class="definition-pill">${escapeHtml(defLangNorm(row.language || 'en'))}</span>
      <span class="definition-pill">${escapeHtml(defSourceLabel(row))}</span>
      ${statusPill}
      ${row.__live && row.contributor_id ? '<span class="definition-pill">Community submitted</span>' : ''}
    </div>
    <div class="definition-full-text">${escapeHtml(row.definition || row.definition_text || 'No definition text available.')}</div>
    <div class="definition-edge-box">
      <strong>Metadata</strong><br>
      Definition ID: ${escapeHtml(row.def_ID || liveId || row.id)}<br>
      Canonical term: ${escapeHtml(row.concept || row.__term?.name_en || '')}<br>
      Source term: ${escapeHtml(row.source_term_label || row.term || '')}<br>
      Citation: ${escapeHtml(row.full_citation || [row.citation_author, row.citation_year, row.citation_title].filter(Boolean).join(' — ') || 'Not available')}<br>
      Contributor: ${contributorHtml(row)}<br>
      Source checks: ${sourceCheck} match / ${mismatch} mismatch / ${noAccess} inaccessible<br>
      Annotation records: ${annotations.length}
    </div>
    ${link ? `<p style="margin-top:.75rem;"><a href="${escapeHtml(link)}" target="_blank" rel="noopener" style="text-decoration:underline;color:var(--green-mid);">Open source ↗</a></p>` : ''}
    <div class="definition-action-row">
      ${canAnnotate ? `<button class="definition-mini-btn" onclick="openAnnotate(event, '${escapeHtml(liveId)}')">Annotate / source-check</button>` : `<button class="definition-mini-btn secondary" disabled title="This legacy node has no Supabase UUID yet.">Legacy node: annotation needs Supabase mapping</button>`}
    </div>`;
}

function defSourceDetailHtml(row) {
  if (!row) return '';
  const parts = [];
  const title = row.source_work_title || row.citation_title || '';
  const book  = row.source_book_title || '';
  const link  = row.hyperlink || row.citation_url || row.source_work_url || row.citUrl || '';
  const workUrl = row.source_work_url || '';
  const author = row.citation_author || '';
  const year   = row.citation_year   || '';
  const doi    = row.citation_doi    || '';
  if (author || year) parts.push(escapeHtml([author, year].filter(Boolean).join(', ')));
  if (title)  parts.push('<em>' + escapeHtml(title) + '</em>');
  if (book)   parts.push('In: <em>' + escapeHtml(book) + '</em>');
  if (doi)    parts.push('<a href="https://doi.org/' + escapeHtml(doi) + '" target="_blank" rel="noopener" style="color:#3d6b59;text-decoration:underline;">DOI ↗</a>');
  if (workUrl && workUrl !== link) parts.push('<a href="' + escapeHtml(workUrl) + '" target="_blank" rel="noopener" style="color:#3d6b59;text-decoration:underline;">Chapter source ↗</a>');
  if (link)   parts.push('<a href="' + escapeHtml(link) + '" target="_blank" rel="noopener" style="color:#3d6b59;font-weight:600;text-decoration:underline;">' + escapeHtml(defSourceLabel(row)) + ' ↗</a>');
  return parts.join('<br>');
}

function defCompareCard(row, label) {
  if (!row) return '';
  const clusterName = row.cluster_name || '';
  const palette = ['#3d6b59','#7a9e7e','#d88700','#9b5de5','#00a6a6','#ef476f','#577590','#bc6c25','#6d597a','#2a9d8f'];
  let clusterColor = '#d6e4dc';
  if (clusterName) {
    let h = 0; String(clusterName).split('').forEach(ch => h = (h * 31 + ch.charCodeAt(0)) >>> 0);
    clusterColor = palette[h % palette.length] + '33';
  }
  return '<div class="definition-edge-box" style="border-left:3px solid ' + (clusterName ? palette[(() => { let h=0; String(clusterName).split('').forEach(ch=>h=(h*31+ch.charCodeAt(0))>>>0); return h; })() % palette.length] : '#999') + ';background:' + clusterColor + ';margin-bottom:.75rem;">'
    + '<div style="font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:#4a5e54;margin-bottom:.35rem;">' + label + '</div>'
    + '<div style="font-size:.8rem;font-weight:700;color:#1c2b24;margin-bottom:.3rem;">' + escapeHtml(row.def_ID || row.id || '') + '</div>'
    + '<div style="font-size:.85rem;line-height:1.65;color:#1c2b24;margin-bottom:.5rem;">' + escapeHtml(row.definition || row.definition_text || '') + '</div>'
    + '<div style="font-size:.75rem;color:#4a5e54;line-height:1.7;">'
    + (clusterName ? '<span style="display:inline-block;margin-bottom:.2rem;font-weight:600;">Cluster: ' + escapeHtml(clusterName) + '</span><br>' : '')
    + defSourceDetailHtml(row)
    + '</div>'
    + '</div>';
}

function showDefinitionEdgeDetails(edgeId) {
  const edge = currentDefinitionEdges.get(edgeId);
  const detail = document.getElementById('definitionDetailPanel');
  if (!edge || !detail) return;
  const a = currentDefinitionGraphItems.get(edge.from);
  const b = currentDefinitionGraphItems.get(edge.to);
  const sim = edge.__data ? Number(edge.__data.cosine_similarity).toFixed(4) : 'n/a';
  const simPct = edge.__data ? Math.round(Number(edge.__data.cosine_similarity) * 100) : null;
  detail.innerHTML = '<h3>Definition comparison</h3>'
    + '<div class="definition-detail-meta">'
    + '<span class="definition-pill">SBERT cosine similarity: <strong>' + sim + '</strong>' + (simPct !== null ? ' (' + simPct + '% semantic overlap)' : '') + '</span>'
    + '</div>'
    + '<p style="font-size:.77rem;color:#8aa09a;margin:.3rem 0 .75rem;">Two definitions are connected by this edge because their SBERT sentence-embedding vectors have a cosine similarity of ' + sim + '. Click either node for full metadata.</p>'
    + defCompareCard(a, 'Definition A')
    + defCompareCard(b, 'Definition B');
}



// ── Static Word-Level Analysis ──────────────────────────────
let wordLevelData = null;
let wordNetwork = null;
let wordLevelInitialised = false;

async function initWordLevelPage() {
  if (wordLevelInitialised) return;
  wordLevelInitialised = true;
  const box = document.getElementById('wordVisual');
  try {
    const res = await fetch('data/word_level_data.json', { cache: 'no-store' });
    if (!res.ok) throw new Error('Could not load word_level_data.json');
    wordLevelData = await res.json();
    const terms = Object.keys(wordLevelData).sort((a,b)=>a.localeCompare(b));
    const sel = document.getElementById('wordTermSelect');
    if (sel) {
      sel.innerHTML = terms.map(t => `<option value="${escapeHtml(t)}">${escapeHtml(t)}</option>`).join('');
      if (terms.includes('Academic Impact')) sel.value = 'Academic Impact';
      else if (terms.length) sel.value = terms[0];
    }
    renderWordLevel();
  } catch (err) {
    console.error(err);
    if (box) box.innerHTML = '<div class="definition-loading">Could not load word-level JSON. Make sure <code>data/word_level_data.json</code> is uploaded next to this HTML file.</div>';
  }
}

function renderWordLevel() {
  if (!wordLevelData) return;
  const term = document.getElementById('wordTermSelect')?.value || '';
  const mode = document.getElementById('wordViewMode')?.value || 'auto';
  const threshold = Math.max(1, Number(document.getElementById('wordThreshold')?.value || 2));
  const maxNodes = Math.max(10, Number(document.getElementById('wordMaxNodes')?.value || 60));
  const data = wordLevelData[term];
  const title = document.getElementById('wordVisualTitle');
  const box = document.getElementById('wordVisual');
  const summary = document.getElementById('wordSummary');
  const topList = document.getElementById('wordTopList');
  if (!data || !box) return;
  if (title) title.textContent = `${term}: ${data.n_definitions} definition${data.n_definitions === 1 ? '' : 's'}`;
  if (summary) summary.innerHTML = `
    <p><strong>${escapeHtml(term)}</strong></p>
    <p>${Number(data.n_definitions||0)} total definitions in the legacy dataset; ${Number(data.n_usable_definitions||0)} have cleaned text for word-level analysis.</p>
    <p>The frequency chart shows the most frequent cleaned words. The network connects words that appear in the same definition.</p>`;
  if (topList) {
    topList.innerHTML = (data.frequencies || []).slice(0, 25).map(x => `
      <div class="word-chip-row"><strong>${escapeHtml(x.word)}</strong><span>${Number(x.n||0)}</span></div>`).join('') || '<p class="word-summary-text">No words available.</p>';
  }
  const usableEdges = (data.edges || []).filter(e => Number(e.n) >= threshold);
  const effectiveMode = mode === 'auto' ? ((Number(data.n_usable_definitions||0) <= 1 || usableEdges.length === 0) ? 'frequency' : 'network') : mode;
  if (effectiveMode === 'frequency') renderWordFrequency(data, term);
  else renderWordNetwork(data, term, threshold, maxNodes);
}

function renderWordFrequency(data, term) {
  const box = document.getElementById('wordVisual');
  if (!box) return;
  if (wordNetwork) { wordNetwork.destroy(); wordNetwork = null; }
  box.innerHTML = '';
  const freqs = (data.frequencies || []).slice(0, 30).reverse();
  if (!freqs.length) {
    box.innerHTML = '<div class="definition-loading">No token frequencies available for this term.</div>';
    return;
  }
  const trace = {
    x: freqs.map(d => d.n),
    y: freqs.map(d => d.word),
    type: 'bar',
    orientation: 'h',
    hovertemplate: '%{y}: %{x}<extra></extra>'
  };
  const layout = {
    title: { text: `Top words for ${term}`, font: { size: 16 } },
    margin: { l: 120, r: 20, t: 50, b: 45 },
    xaxis: { title: 'Frequency' },
    yaxis: { automargin: true },
    paper_bgcolor: '#fff',
    plot_bgcolor: '#fff'
  };
  Plotly.newPlot(box, [trace], layout, { responsive: true, displayModeBar: true });
}

function renderWordNetwork(data, term, threshold, maxNodes) {
  const box = document.getElementById('wordVisual');
  if (!box) return;
  if (window.Plotly) { try { Plotly.purge(box); } catch(e) {} }
  const edgesRaw = (data.edges || []).filter(e => Number(e.n) >= threshold);
  if (!edgesRaw.length) {
    box.innerHTML = '<div class="definition-loading">No co-occurrences at this threshold. Try lowering the minimum co-occurrence.</div>';
    return;
  }
  const freqMap = new Map((data.frequencies || []).map(d => [d.word, Number(d.n||0)]));
  const allowed = new Set((data.frequencies || []).slice(0, maxNodes).map(d => d.word));
  const edges = edgesRaw.filter(e => allowed.has(e.from) && allowed.has(e.to)).slice(0, 600);
  const nodeSet = new Set();
  edges.forEach(e => { nodeSet.add(e.from); nodeSet.add(e.to); });
  if (!nodeSet.size) {
    box.innerHTML = '<div class="definition-loading">No network after applying the maximum-word limit. Increase “Maximum words”.</div>';
    return;
  }
  // Get cluster color for this term
  const termCluster = getClusterForTerm(term);
  const termClusterBg = termCluster ? termClusterColor(termCluster) : '#2d4a3e';
  const termClusterLight = termCluster ? termClusterColor(termCluster) + '40' : '#dceee5';

  const nodes = Array.from(nodeSet).map(w => ({
    id: w, label: w,
    value: Math.max(5, freqMap.get(w) || 1),
    title: `${w}\nFrequency: ${freqMap.get(w) || 0}`,
    color: { background: termClusterLight, border: termClusterBg, highlight: { background: '#fff3cd', border: '#d88700' } }
  }));
  const visEdges = edges.map((e,i) => ({
    id: `wedge:${i}`, from: e.from, to: e.to,
    value: Number(e.n), width: Math.max(1, Math.sqrt(Number(e.n))*2),
    title: `${e.from} + ${e.to}
Co-occurs in ${e.n} definition${Number(e.n) === 1 ? '' : 's'}`
  }));
  // Update cluster legend
  const legendDiv = document.getElementById('wordClusterLegend');
  const legendDot = document.getElementById('wordClusterDot');
  const legendName = document.getElementById('wordClusterName');
  if (legendDiv && termCluster) {
    legendDiv.style.display = 'flex';
    if (legendDot) legendDot.style.background = termClusterBg;
    if (legendName) legendName.textContent = termCluster;
  } else if (legendDiv) {
    legendDiv.style.display = 'none';
  }

  box.innerHTML = '';
  box.style.height = '520px';
  const networkData = { nodes: new vis.DataSet(nodes), edges: new vis.DataSet(visEdges) };
  const options = {
    interaction: { hover: true, tooltipDelay: 80, navigationButtons: true, keyboard: true },
    physics: { enabled: true, solver: 'forceAtlas2Based', stabilization: { iterations: 160 } },
    nodes: { shape: 'dot', color: { background: '#dceee5', border: '#2d4a3e', highlight: { background: '#fff3cd', border: '#d88700' } }, font: { size: 16 } },
    edges: { color: { color: '#8fa19a', highlight: '#d88700' }, smooth: { type: 'dynamic' }, scaling: { min: 1, max: 10 } }
  };
  wordNetwork = new vis.Network(box, networkData, options);
}

// Extend existing page navigation to initialise the graph lazily.
(function(){
  const oldShowPage = window.showPage;
  window.showPage = function(name, evt) {
    const r = oldShowPage ? oldShowPage(name, evt) : undefined;
    if (name === 'definitions') setTimeout(initDefinitionNetworkPage, 0);
    if (name === 'words') setTimeout(initWordLevelPage, 0);
    return r;
  };
  window.initDefinitionNetworkPage = initDefinitionNetworkPage;
  window.renderDefinitionNetwork = renderDefinitionNetwork;
  window.initWordLevelPage = initWordLevelPage;
  window.renderWordLevel = renderWordLevel;
})();

/* ─────────────────────────────────────────────────────────────
   Static Term-Level Analysis converted from Shiny
   Modes: word co-occurrence network, clustering graph, types/tokens/TTR
   ───────────────────────────────────────────────────────────── */
let termLevelData = null;
let termCooccurrenceNetwork = null;
let termLevelInitialised = false;

function escapeHtmlTerm(x) {
  return String(x ?? '').replace(/[&<>"']/g, m => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[m]));
}
function getClusterForTerm(termName) {
  if (!termLevelData || !termLevelData.cluster_points) return null;
  const pt = termLevelData.cluster_points.find(p => p.term && p.term.toLowerCase() === String(termName||'').toLowerCase());
  return pt ? (pt.cluster_name || null) : null;
}

function termClusterColor(name) {
  const palette = ['#3d6b59','#7a9e7e','#d88700','#9b5de5','#00a6a6','#ef476f','#577590','#bc6c25','#6d597a','#2a9d8f'];
  let h = 0; String(name || '').split('').forEach(ch => h = (h * 31 + ch.charCodeAt(0)) >>> 0);
  return palette[h % palette.length];
}
async function initTermLevelPage() {
  if (termLevelInitialised) return;
  termLevelInitialised = true;
  try {
    const res = await fetch('data/term_level_data.json', { cache: 'no-store' });
    if (!res.ok) throw new Error('Could not load term_level_data.json');
    termLevelData = await res.json();
    populateTermLevelControls();
    const route = location.hash.replace(/^#/, '').split('?');
    if (route[0] === 'terms') {
      const params = new URLSearchParams(route[1] || '');
      const t = termFromKey(params.get('term'));
      if (t?.name) ['termCooccurrenceSelect','termClusterSelect'].forEach(id => { const el=document.getElementById(id); if(el) el.value=t.name; });
      const mode = params.get('mode'); if (mode) populateClusterFilters();
  switchTermMode(mode);
    }
    if (termLevelData.note) {
      const note = document.getElementById('termLevelNote');
      if (note) { note.style.display = 'block'; note.textContent = termLevelData.note; }
    }
    renderTermCooccurrence();
  } catch (err) {
    ['termCooccurrenceNetwork','termClusterPlot','typesTokensPlot'].forEach(id => {
      const el = document.getElementById(id);
      if (el) el.innerHTML = '<div class="definition-loading">Could not load term-level JSON. Make sure data/term_level_data.json is uploaded next to this HTML file.</div>';
    });
    console.error(err);
  }
}
function populateTermLevelControls() {
  const terms = (termLevelData?.terms || []).slice().sort((a,b) => a.term.localeCompare(b.term));
  const options = '<option value="">Select a term…</option>' + terms.map(t => `<option value="${escapeHtmlTerm(t.term)}">${escapeHtmlTerm(t.term)}</option>`).join('');
  ['termCooccurrenceSelect','termClusterSelect'].forEach(id => { const el = document.getElementById(id); if (el) el.innerHTML = options; });
  const maxTypes = Math.max(...terms.map(t => Number(t.types)||0), 1);
  const maxTokens = Math.max(...terms.map(t => Number(t.tokens)||0), 1);
  const maxTtr = Math.max(...terms.map(t => Number(t.type_to_token_ratio)||0), 1);
  const setVal = (id,v) => { const el=document.getElementById(id); if(el) el.value=v; };
  setVal('typesMin', 0); setVal('typesMax', maxTypes);
  setVal('tokensMin', 0); setVal('tokensMax', maxTokens);
  setVal('ttrMin', 0); setVal('ttrMax', Math.min(1, Math.ceil(maxTtr*100)/100));
}
function switchTermMode(mode) {
  document.querySelectorAll('.term-mode-btn').forEach(b => b.classList.toggle('active', b.dataset.termMode === mode));
  document.querySelectorAll('.term-mode-panel').forEach(p => p.classList.remove('active'));
  const panel = document.getElementById('termMode-' + mode); if (panel) panel.classList.add('active');
  setTimeout(() => {
    if (mode === 'cooccurrence') renderTermCooccurrence();
    if (mode === 'clustering') renderTermClustering();
    if (mode === 'typestokens') renderTypesTokens();
  }, 0);
}
function renderTermCooccurrence() {
  if (!termLevelData) return;
  const selected = document.getElementById('termCooccurrenceSelect')?.value || '';
  const threshold = Number(document.getElementById('termCooccurrenceThreshold')?.value || 25);
  const maxEdges = Number(document.getElementById('termCooccurrenceMaxEdges')?.value || 500);
  const box = document.getElementById('termCooccurrenceNetwork');
  const table = document.getElementById('termCooccurrenceTable');
  if (!box) return;
  let edges = (termLevelData.term_edges || []).filter(e => Number(e.weight) >= threshold);
  if (selected) edges = edges.filter(e => e.from === selected || e.to === selected);
  edges = edges.sort((a,b) => Number(b.weight)-Number(a.weight)).slice(0, maxEdges);
  if (!edges.length) {
    box.innerHTML = '<div class="definition-loading">No links at this threshold. Lower the minimum co-occurrence value.</div>';
    if (table) table.innerHTML = '';
    return;
  }
  const termsByName = new Map((termLevelData.terms||[]).map(t => [t.term, t]));
  const ids = new Set(); edges.forEach(e => { ids.add(e.from); ids.add(e.to); });
  if (selected) ids.add(selected);
  const nodes = Array.from(ids).map(id => {
    const t = termsByName.get(id) || {};
    return { id, label: id, shape: id === selected ? 'star' : 'dot', size: id === selected ? 28 : 16,
      color: { background: termClusterColor(t.cluster_name), border: id === selected ? '#d88700' : '#2d4a3e' },
      title: `${id}\nCluster: ${t.cluster_name || 'Unknown'}\nDefinitions: ${t.total_definitions || 0}` };
  });
  const visEdges = edges.map((e,i) => ({ id:'te'+i, from:e.from, to:e.to, value:Number(e.weight), width:Math.max(1, Math.sqrt(Number(e.weight))*0.8), title:`${e.from} + ${e.to}\nShared words: ${e.weight}` }));
  box.innerHTML = '';
  box.style.height = '620px';
  termCooccurrenceNetwork = new vis.Network(box, { nodes:new vis.DataSet(nodes), edges:new vis.DataSet(visEdges) }, {
    interaction:{ hover:true, tooltipDelay:90, navigationButtons:true, keyboard:true },
    physics:{ enabled:true, solver:'forceAtlas2Based', stabilization:{ iterations:180 } },
    nodes:{ font:{ size:14 }, borderWidth:1.5 },
    edges:{ color:{ color:'#8fa19a', highlight:'#d88700' }, smooth:{ type:'dynamic' }, scaling:{ min:1, max:10 } }
  });
  if (selected && table) {
    const connected = edges.map(e => ({ term: e.from === selected ? e.to : e.from, weight: e.weight })).sort((a,b)=>b.weight-a.weight).slice(0,50);
    table.innerHTML = connected.length ? `<table><thead><tr><th>Term</th><th>Shared words</th></tr></thead><tbody>${connected.map(r=>`<tr><td>${escapeHtmlTerm(r.term)}</td><td>${r.weight}</td></tr>`).join('')}</tbody></table>` : '<p class="term-side-help">No connected terms.</p>';
  } else if (table) {
    table.innerHTML = '<p class="term-side-help">Select a term to see its connected terms.</p>';
  }
}
function renderTermClustering() {
  if (!termLevelData || !window.Plotly) return;
  const selected = document.getElementById('termClusterSelect')?.value || '';
  const points = termLevelData.cluster_points || [];
  const clusters = [...new Set(points.map(p => p.cluster_name || 'Unclustered'))].sort();
  const traces = clusters.map(cl => {
    const d = points.filter(p => (p.cluster_name || 'Unclustered') === cl && p.term !== selected);
    return { x:d.map(p=>p.x), y:d.map(p=>p.y), text:d.map(p=>`${p.term}<br>${p.cluster_name}`), hoverinfo:'text', mode:'markers', type:'scatter', name:cl, marker:{ size:9, opacity:0.72 } };
  });
  if (selected) {
    const s = points.find(p => p.term === selected);
    if (s) traces.push({ x:[s.x], y:[s.y], text:[`${s.term}<br>${s.cluster_name}`], hoverinfo:'text', mode:'markers+text', type:'scatter', name:'Selected term', textposition:'top center', marker:{ size:18, symbol:'star', line:{ width:2, color:'#1c2b24' } } });
  }
  Plotly.newPlot('termClusterPlot', traces, { title:'Clustering graph of terms', xaxis:{ title:'UMAP Dimension 1 (lexical structure)', zeroline:false }, yaxis:{ title:'UMAP Dimension 2 (lexical structure)', zeroline:false }, margin:{ t:45,l:45,r:20,b:45 }, legend:{ orientation:'h' } }, { responsive:true, displaylogo:false });
  const table = document.getElementById('termClosestTable');
  if (table) {
    if (!selected) { table.innerHTML = '<p class="term-side-help">Select a term to see closest terms.</p>'; return; }
    const s = points.find(p => p.term === selected);
    const rows = (s?.closest_terms || []).slice(0,20);
    table.innerHTML = rows.length ? `<table><thead><tr><th>Term</th><th>Overlap</th></tr></thead><tbody>${rows.map(r=>`<tr><td>${escapeHtmlTerm(r.term)}<br><small>${escapeHtmlTerm(r.cluster_name||'')}</small></td><td>${r.similarity}</td></tr>`).join('')}</tbody></table>` : '<p class="term-side-help">No closest terms available.</p>';
  }
}
function renderTypesTokens() {
  if (!termLevelData || !window.Plotly) return;
  const n = id => Number(document.getElementById(id)?.value || 0);
  const minTypes=n('typesMin'), maxTypes=n('typesMax'), minTokens=n('tokensMin'), maxTokens=n('tokensMax'), minTtr=n('ttrMin'), maxTtr=n('ttrMax');
  let rows = (termLevelData.terms || []).filter(t => Number(t.types)>=minTypes && Number(t.types)<=maxTypes && Number(t.tokens)>=minTokens && Number(t.tokens)<=maxTokens && Number(t.type_to_token_ratio)>=minTtr && Number(t.type_to_token_ratio)<=maxTtr);
  const clusters = [...new Set(rows.map(r => r.cluster_name || 'Unclustered'))].sort();
  const traces = clusters.map(cl => {
    const d=rows.filter(r => (r.cluster_name || 'Unclustered') === cl);
    return { x:d.map(r=>r.tokens), y:d.map(r=>r.types), z:d.map(r=>r.type_to_token_ratio), text:d.map(r=>`${r.term}<br>Cluster: ${r.cluster_name}<br>Definitions: ${r.total_definitions}<br>Avg similarity: ${r.avg_similarity ?? '—'}`), hoverinfo:'text', mode:'markers', type:'scatter3d', name:cl, marker:{ size:5, opacity:0.82 } };
  });
  Plotly.newPlot('typesTokensPlot', traces, { title:'Types, tokens, and type-to-token ratio', margin:{ t:45,l:0,r:0,b:0 }, scene:{ xaxis:{title:'Tokens'}, yaxis:{title:'Types'}, zaxis:{title:'TTR'} }, legend:{ orientation:'h' } }, { responsive:true, displaylogo:false });
  const table=document.getElementById('typesTokensTable');
  if (table) {
    rows = rows.sort((a,b)=>Number(b.total_definitions)-Number(a.total_definitions)).slice(0,60);
    table.innerHTML = `<table><thead><tr><th>Term</th><th>Types</th><th>Tokens</th><th>TTR</th></tr></thead><tbody>${rows.map(r=>`<tr><td>${escapeHtmlTerm(r.term)}<br><small>${escapeHtmlTerm(r.cluster_name||'')}</small></td><td>${r.types}</td><td>${r.tokens}</td><td>${r.type_to_token_ratio ?? '—'}</td></tr>`).join('')}</tbody></table>`;
  }
}
(function(){
  const previousShowPage = window.showPage;
  window.showPage = function(name, evt) {
    const result = previousShowPage ? previousShowPage(name, evt) : undefined;
    if (name === 'terms') setTimeout(initTermLevelPage, 0);
    return result;
  };
  window.initTermLevelPage = initTermLevelPage;
  window.switchTermMode = switchTermMode;
  window.renderTermCooccurrence = renderTermCooccurrence;
  window.renderTermClustering = renderTermClustering;
  window.renderTypesTokens = renderTypesTokens;
})();