/*
author: Daniel Goertzen <daniel.goertzen@gmail.com>
copyright: 2012 Daniel Goertzen
license: See file /LICENSE
*/

#include "erl_nif.h"
#include "libyaml/yaml.h"

static const char *encoding_atoms[] = {"any", "utf8", "utf16le", "utf16be"};
//static const char *break_atoms[] = {"any", "cr", "ln", "crln"};
static const char *error_type_atoms[] = {"no", "memory", "reader", "scanner", "parser", "composer", "writer", "emitter"};
static const char *scalar_style_atoms[] = {"any", "plain", "single_quoted", "double_quoted", "literal", "folded"};
static const char *sequence_style_atoms[] = {"any", "block", "flow"};
static const char *mapping_style_atoms[] = {"any", "block", "flow"};
static const char *event_type_atoms[] = {
    "no",
    "stream_start", "stream_end",
    "document_start", "document_end",
    "alias", "scalar",
    "sequence_start", "sequence_end",
    "mapping_start", "mapping_end"};

static ERL_NIF_TERM enum_to_atom(ErlNifEnv *env, int value, const char*table[])
{
    return enif_make_atom(env, table[value]);
}

static ERL_NIF_TERM mem_to_binary(ErlNifEnv *env, const unsigned char *cstr, size_t len)
{
    unsigned char *bin;
    ERL_NIF_TERM term;

    if(cstr)
    {
        bin = enif_make_new_binary(env, len, &term);
        memcpy(bin, cstr, len);
        return term;
    }
    else
    {
        return enif_make_atom(env, "null");
    }
}

static ERL_NIF_TERM cstr_to_binary(ErlNifEnv *env, const unsigned char *cstr)
{
    if(cstr)
    {
        return mem_to_binary(env, cstr, strlen(cstr));
    }
    else
    {
        return enif_make_atom(env, "null");
    }
}


static ERL_NIF_TERM bool_to_term(ErlNifEnv *env, int value)
{
    return enif_make_atom(env, value?"true":"false");
}

static ERL_NIF_TERM version_directive_to_term(ErlNifEnv *env, yaml_version_directive_t *ver)
{
    if(ver)
    {
        return enif_make_tuple2(env,
                                enif_make_int(env, ver->major),
                                enif_make_int(env, ver->minor));
    }
    else
    {
        return enif_make_atom(env, "null");
    }
}

static ERL_NIF_TERM tag_directive_to_term(ErlNifEnv *env, yaml_tag_directive_t *tag)
{
        return enif_make_tuple2(env,
                                cstr_to_binary(env, tag->handle),
                                cstr_to_binary(env, tag->prefix));
}

static ERL_NIF_TERM mark_to_term(ErlNifEnv *env, yaml_mark_t *mark)
{
    if(mark)
    {
        return enif_make_tuple3(env,
                                enif_make_ulong(env, mark->index),
                                enif_make_ulong(env, mark->line),
                                enif_make_ulong(env, mark->column));
    }
    else
    {
        return enif_make_atom(env, "null");
    }
}


static ERL_NIF_TERM event_to_term(ErlNifEnv *env, yaml_event_t *event)
{
    yaml_event_type_t type;
    ERL_NIF_TERM term;
    ERL_NIF_TERM tail;

    // for iteration
    yaml_tag_directive_t *tag_directive;

    type = event->type;
    switch(type)
    {
    case YAML_STREAM_START_EVENT:
        term = enum_to_atom(env, event->data.stream_start.encoding, encoding_atoms);
        break;

    case YAML_DOCUMENT_START_EVENT:
        // form list of directives
        tail = enif_make_list(env,0);

        // iterate backwards so list ends up in right order
        tag_directive = event->data.document_start.tag_directives.end;
        while(event->data.document_start.tag_directives.start != tag_directive)
        {
            tail = enif_make_list_cell(env, tag_directive_to_term(env, tag_directive), tail);
            tag_directive--;
        }
        term = enif_make_tuple3(env,
                                version_directive_to_term(env, event->data.document_start.version_directive),
                                tail,
                                bool_to_term(env, event->data.document_start.implicit));
        break;


    case YAML_DOCUMENT_END_EVENT:
        term = bool_to_term(env, event->data.document_end.implicit);
        break;

    case YAML_ALIAS_EVENT:
        term = cstr_to_binary(env, event->data.alias.anchor);
        break;


    case YAML_SCALAR_EVENT:
        term = enif_make_tuple4(env,
                                cstr_to_binary(env, event->data.scalar.anchor),
                                cstr_to_binary(env, event->data.scalar.tag),
                                mem_to_binary(env, event->data.scalar.value, event->data.scalar.length),
//                                bool_to_term(env, event->data.scalar.plain_implicit),
//                                bool_to_term(env, event->data.scalar.quoted_implicit),
                                enum_to_atom(env, event->data.scalar.style, scalar_style_atoms));
        break;


    case YAML_SEQUENCE_START_EVENT:
        term = enif_make_tuple3(env,
                                cstr_to_binary(env, event->data.sequence_start.anchor),
                                cstr_to_binary(env, event->data.sequence_start.tag),
//                                bool_to_term(env, event->data.sequence_start.implicit),
                                enum_to_atom(env, event->data.sequence_start.style, sequence_style_atoms));
        break;

    case YAML_MAPPING_START_EVENT:
        term = enif_make_tuple3(env,
                                cstr_to_binary(env, event->data.mapping_start.anchor),
                                cstr_to_binary(env, event->data.mapping_start.tag),
//                                bool_to_term(env, event->data.mapping_start.implicit),
                                enum_to_atom(env, event->data.mapping_start.style, mapping_style_atoms));
        break;

    default:
        term = enif_make_atom(env, "null");
        break;
    }

    return enif_make_tuple4(env,
                            enum_to_atom(env, type, event_type_atoms),
                            term,
                            mark_to_term(env, &event->start_mark),
                            mark_to_term(env, &event->end_mark));
}



static ERL_NIF_TERM binary_to_libyaml_event_stream_rev(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    ERL_NIF_TERM tail;
    yaml_parser_t parser;
    yaml_event_t event;
    yaml_error_type_t error;
    int done = 0;
    char msg[200];

    if (!enif_inspect_binary(env, argv[0], &bin))
    {
        return enif_make_badarg(env);
    }

    tail = enif_make_list(env,0);

    /* Create the Parser object. */
    yaml_parser_initialize(&parser);

    yaml_parser_set_input_string(&parser, bin.data, bin.size);

    /* Read the event sequence. */
    while (!done) {

        /* Get the next event. */
        if (!yaml_parser_parse(&parser, &event))
            goto parser_error;

        // convert and add to list
        tail = enif_make_list_cell(env, event_to_term(env,&event), tail);

        /* Are we finished? */
        done = (event.type == YAML_STREAM_END_EVENT);

        /* The application is responsible for destroying the event object. */
        yaml_event_delete(&event);

    }

    /* Destroy the Parser object. */
    yaml_parser_delete(&parser);

    return enif_make_tuple2(env,
                            enif_make_atom(env, "ok"),
                            tail);

    /* On error. */
parser_error:
    memset(msg,0, sizeof(msg));

    error = parser.error;
    switch (error)
    {
    case YAML_MEMORY_ERROR:
        snprintf(msg, sizeof(msg), "Memory error: Not enough memory for parsing\n");
        break;

    case YAML_READER_ERROR:
        if (parser.problem_value != -1) {
            snprintf(msg, sizeof(msg), "Reader error: %s: #%X at %zu\n", parser.problem,
                     parser.problem_value, parser.problem_offset);
        }
        else {
            fprintf(stderr, "Reader error: %s at %zu\n", parser.problem,
                    parser.problem_offset);
        }
        break;

    case YAML_SCANNER_ERROR:
        if (parser.context) {
            snprintf(msg, sizeof(msg), "Scanner error: %s at line %zu, column %zu\n"
                     "%s at line %zu, column %zu\n", parser.context,
                     parser.context_mark.line+1, parser.context_mark.column+1,
                     parser.problem, parser.problem_mark.line+1,
                     parser.problem_mark.column+1);
        }
        else {
            snprintf(msg, sizeof(msg), "Scanner error: %s at line %zu, column %zu\n",
                     parser.problem, parser.problem_mark.line+1,
                     parser.problem_mark.column+1);
        }
        break;

    case YAML_PARSER_ERROR:
        if (parser.context) {
            snprintf(msg, sizeof(msg), "Parser error: %s at line %zu, column %zu\n"
                     "%s at line %zu, column %zu\n", parser.context,
                     parser.context_mark.line+1, parser.context_mark.column+1,
                     parser.problem, parser.problem_mark.line+1,
                     parser.problem_mark.column+1);
        }
        else {
            snprintf(msg, sizeof(msg), "Parser error: %s at line %zu, column %zu\n",
                     parser.problem, parser.problem_mark.line+1,
                     parser.problem_mark.column+1);
        }
        break;
    default:
    	break;
    }



    /* Destroy the Parser object. */
    yaml_parser_delete(&parser);

    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_tuple2(env,
                                             enum_to_atom(env, error, error_type_atoms),
                                             cstr_to_binary(env,msg)));

}

static ErlNifFunc nif_funcs[] = {
    {"binary_to_libyaml_event_stream_rev", 1, binary_to_libyaml_event_stream_rev}
};
ERL_NIF_INIT(yaml_libyaml, nif_funcs, NULL, NULL, NULL, NULL)

