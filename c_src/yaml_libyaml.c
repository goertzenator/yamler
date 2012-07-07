/**
 * author: Daniel Goertzen <daniel.goertzen@gmail.com>
 * contributor: Sergei Lebedev <superbobry@gmail.com>
 * copyright: 2012 Daniel Goertzen
 * license: See file /LICENSE
 */

#include "erl_nif.h"
#include <yaml.h>

#define ATOM(s)         enif_make_atom(env, s)
#define BOOL(v)         ATOM(v ? "true" : "false")
#define INT(n)          enif_make_int(env, n)
#define ULONG(ln)       enif_make_ulong(env, ln)
#define TUPLE2(x, y)    enif_make_tuple2(env, x, y)
#define TUPLE3(x, y, z) enif_make_tuple3(env, x, y, z)
#define TUPLE4(x, y, z, a) enif_make_tuple4(env, x, y, z, a)
#define BINARY(s)       (!s ? ATOM("null") : MEMORY(s, strlen((const char *) s)))
#define MEMORY(s, len)  enif_wrap_binary(env, s, len)
#define ENUM(table, value) ATOM(table[value])

#define UNUSED  __attribute__((unused))

static const char *encodings[] = {"any", "utf8", "utf16le", "utf16be"};
static const char *breaks[] UNUSED = {"any", "cr", "ln", "crln"};
static const char *error_types[] = {
    "no", "memory",
    "reader", "scanner", "parser",
    "composer", "writer", "emitter"
};
static const char *scalar_styles[] = {
    "any", "plain", "single_quoted", "double_quoted", "literal", "folded"
};
static const char *sequence_styles[] = {"any", "block", "flow"};
static const char *mapping_styles[]  = {"any", "block", "flow"};
static const char *event_types[] = {
    "no",
    "stream_start", "stream_end",
    "document_start", "document_end",
    "alias", "scalar",
    "sequence_start", "sequence_end",
    "mapping_start", "mapping_end"
};


static inline ERL_NIF_TERM
enif_wrap_binary(ErlNifEnv *env, const unsigned char *cstr, const size_t len)
{
    unsigned char *bin;
    ERL_NIF_TERM term;

    if (!cstr)
        term = ATOM("null");
    else {
        bin = enif_make_new_binary(env, len, &term);
        memcpy(bin, cstr, len);
    }

    return term;
}


static ERL_NIF_TERM
version_directive_to_term(ErlNifEnv *env, const yaml_version_directive_t *version)
{
    if (!version)
        return ATOM("null");
    else
        return TUPLE2(INT(version->major), INT(version->minor));
}


static inline ERL_NIF_TERM
tag_directive_to_term(ErlNifEnv *env, yaml_tag_directive_t *tag)
{
    return TUPLE2(BINARY(tag->handle),
                  BINARY(tag->prefix));
}

static inline ERL_NIF_TERM
mark_to_term(ErlNifEnv *env, const yaml_mark_t *mark)
{
    if (!mark)
        return ATOM("null");
    else
        return TUPLE3(ULONG(mark->index), ULONG(mark->line), ULONG(mark->column));
}


static ERL_NIF_TERM event_to_term(ErlNifEnv *env, const yaml_event_t *event)
{
    const yaml_event_type_t type = event->type;
    ERL_NIF_TERM term;

    switch (type) {
    case YAML_STREAM_START_EVENT:
        term = ENUM(encodings, event->data.stream_start.encoding);
        break;
    case YAML_DOCUMENT_START_EVENT:
        term = enif_make_list(env, 0);

        // iterate backwards so list ends up in right order
        for (yaml_tag_directive_t *tag_directive = event->data.document_start.tag_directives.end;
             event->data.document_start.tag_directives.start != tag_directive;
             tag_directive--) {
            term = enif_make_list_cell(
                env, tag_directive_to_term(env, tag_directive), term);
        }

        term = TUPLE3(version_directive_to_term(env, event->data.document_start.version_directive),
                      term,
                      BOOL(event->data.document_start.implicit));
        break;
    case YAML_DOCUMENT_END_EVENT:
        term = BOOL(event->data.document_end.implicit);
        break;
    case YAML_ALIAS_EVENT:
        term = BINARY(event->data.alias.anchor);
        break;
    case YAML_SCALAR_EVENT:
        term = TUPLE4(
            BINARY(event->data.scalar.anchor),
            BINARY(event->data.scalar.tag),
            MEMORY(event->data.scalar.value, event->data.scalar.length),
            /* FIXME(Sergei): why is this commented out?
               BOOL(event->data.scalar.plain_implicit),
               BOOL(event->data.scalar.quoted_implicit), */
            ENUM(scalar_styles, event->data.scalar.style));
        break;
    case YAML_SEQUENCE_START_EVENT:
        term = TUPLE3(
            BINARY(event->data.sequence_start.anchor),
            BINARY(event->data.sequence_start.tag),
            /* FIXME(Sergei): why is this commented out?
              BOOL(event->data.sequence_start.implicit), */
            ENUM(sequence_styles, event->data.sequence_start.style));
        break;
    case YAML_MAPPING_START_EVENT:
        term = TUPLE3(
            BINARY(event->data.mapping_start.anchor),
            BINARY(event->data.mapping_start.tag),
            /* FIXME(Sergei): same for this.
               BOOL(event->data.mapping_start.implicit), */
            ENUM(mapping_styles, event->data.mapping_start.style));
        break;
    default:
        term = ATOM("null");
        break;
    }

    return TUPLE4(ENUM(event_types, type), term,
                  mark_to_term(env, &event->start_mark),
                  mark_to_term(env, &event->end_mark));
}



static ERL_NIF_TERM
binary_to_libyaml_event_stream_rev(ErlNifEnv* env,
                                   int argc UNUSED,
                                   const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM term = enif_make_list(env, 0);
    ERL_NIF_TERM status;
    ErlNifBinary bin;
    yaml_parser_t parser;
    yaml_event_t event;
    int done = 0;
    char msg[256] = {0};

    if (!enif_inspect_binary(env, argv[0], &bin))
        return enif_make_badarg(env);

    yaml_parser_initialize(&parser);
    yaml_parser_set_input_string(&parser, bin.data, bin.size);

    while (!done) {
        if (!yaml_parser_parse(&parser, &event))
            goto parser_error;

        term = enif_make_list_cell(env, event_to_term(env, &event), term);
        done = event.type == YAML_STREAM_END_EVENT;
        yaml_event_delete(&event);
    }

    status = ATOM("ok");
    goto parser_done;

parser_error:
    switch (parser.error) {
    case YAML_MEMORY_ERROR:
        snprintf(msg, sizeof(msg), "Memory error: Not enough memory for parsing");
        break;
    case YAML_READER_ERROR:
        if (parser.problem_value != -1)
            snprintf(msg, sizeof(msg), "Reader error: %s: #%X at %zu",
                     parser.problem,
                     parser.problem_value, parser.problem_offset);
        else
            snprintf(msg, sizeof(msg), "Reader error: %s at %zu",
                     parser.problem,
                     parser.problem_offset);
        break;
    case YAML_SCANNER_ERROR:
        if (parser.context)
            snprintf(msg, sizeof(msg), "Scanner error: %s at line %zu, column %zu\n"
                     "%s at line %zu, column %zu",
                     parser.context,
                     parser.context_mark.line + 1, parser.context_mark.column + 1,
                     parser.problem, parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        else
            snprintf(msg, sizeof(msg), "Scanner error: %s at line %zu, column %zu",
                     parser.problem,
                     parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        break;
    case YAML_PARSER_ERROR:
        if (parser.context)
            snprintf(msg, sizeof(msg), "Parser error: %s at line %zu, column %zu\n"
                     "%s at line %zu, column %zu",
                     parser.context,
                     parser.context_mark.line + 1, parser.context_mark.column + 1,
                     parser.problem, parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        else
            snprintf(msg, sizeof(msg), "Parser error: %s at line %zu, column %zu",
                     parser.problem,
                     parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        break;
    case YAML_NO_ERROR:
    case YAML_COMPOSER_ERROR:
    case YAML_WRITER_ERROR:
    case YAML_EMITTER_ERROR:
        break;
    }

    status = ATOM("error");
    term   = TUPLE2(ENUM(error_types, parser.error),
                    MEMORY((const unsigned char *) msg, strlen(msg)));
    goto parser_done;

parser_done:
    yaml_parser_delete(&parser);
    return TUPLE2(status, term);
}


static ErlNifFunc nif_funcs[] = {
    {"binary_to_libyaml_event_stream_rev", 1, binary_to_libyaml_event_stream_rev}
};


ERL_NIF_INIT(yaml_libyaml, nif_funcs, NULL, NULL, NULL, NULL)
