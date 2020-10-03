# -*- coding: utf-8 -*-
# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: Command.proto
"""Generated protocol buffer code."""
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database

# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()

import ChoicePoint_pb2 as ChoicePoint__pb2
import DataPoint_pb2 as DataPoint__pb2

DESCRIPTOR = _descriptor.FileDescriptor(
    name='Command.proto',
    package='',
    syntax='proto3',
    serialized_options=None,
    create_key=_descriptor._internal_create_key,
    serialized_pb=b'\n\rCommand.proto\x1a\x11\x43hoicePoint.proto\x1a\x0f\x44\x61taPoint.proto\"\r\n\x0bInitCommand\"4\n\x0ePredictCommand\x12\"\n\x0c\x63hoicePoints\x18\x01 \x03(\x0b\x32\x0c.ChoicePoint\"?\n\x0cTrainCommand\x12\x0f\n\x07nEpochs\x18\x01 \x01(\x05\x12\x1e\n\ndatapoints\x18\x02 \x03(\x0b\x32\n.DataPoint\".\n\x0cValidCommand\x12\x1e\n\ndatapoints\x18\x01 \x03(\x0b\x32\n.DataPoint\"\x1f\n\x0bSaveCommand\x12\x10\n\x08\x66ilename\x18\x01 \x01(\t\"\x1f\n\x0bLoadCommand\x12\x10\n\x08\x66ilename\x18\x01 \x01(\t\"\xcf\x01\n\x07\x43ommand\x12\x1c\n\x04init\x18\x01 \x01(\x0b\x32\x0c.InitCommandH\x00\x12\"\n\x07predict\x18\x02 \x01(\x0b\x32\x0f.PredictCommandH\x00\x12\x1e\n\x05train\x18\x03 \x01(\x0b\x32\r.TrainCommandH\x00\x12\x1e\n\x05valid\x18\x04 \x01(\x0b\x32\r.ValidCommandH\x00\x12\x1c\n\x04save\x18\x05 \x01(\x0b\x32\x0c.SaveCommandH\x00\x12\x1c\n\x04load\x18\x06 \x01(\x0b\x32\x0c.LoadCommandH\x00\x42\x06\n\x04\x62odyb\x06proto3'
    ,
    dependencies=[ChoicePoint__pb2.DESCRIPTOR, DataPoint__pb2.DESCRIPTOR, ])

_INITCOMMAND = _descriptor.Descriptor(
    name='InitCommand',
    full_name='InitCommand',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
    ],
    serialized_start=53,
    serialized_end=66,
)

_PREDICTCOMMAND = _descriptor.Descriptor(
    name='PredictCommand',
    full_name='PredictCommand',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
        _descriptor.FieldDescriptor(
            name='choicePoints', full_name='PredictCommand.choicePoints', index=0,
            number=1, type=11, cpp_type=10, label=3,
            has_default_value=False, default_value=[],
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
    ],
    serialized_start=68,
    serialized_end=120,
)

_TRAINCOMMAND = _descriptor.Descriptor(
    name='TrainCommand',
    full_name='TrainCommand',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
        _descriptor.FieldDescriptor(
            name='nEpochs', full_name='TrainCommand.nEpochs', index=0,
            number=1, type=5, cpp_type=1, label=1,
            has_default_value=False, default_value=0,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
        _descriptor.FieldDescriptor(
            name='datapoints', full_name='TrainCommand.datapoints', index=1,
            number=2, type=11, cpp_type=10, label=3,
            has_default_value=False, default_value=[],
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
    ],
    serialized_start=122,
    serialized_end=185,
)

_VALIDCOMMAND = _descriptor.Descriptor(
    name='ValidCommand',
    full_name='ValidCommand',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
        _descriptor.FieldDescriptor(
            name='datapoints', full_name='ValidCommand.datapoints', index=0,
            number=1, type=11, cpp_type=10, label=3,
            has_default_value=False, default_value=[],
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
    ],
    serialized_start=187,
    serialized_end=233,
)

_SAVECOMMAND = _descriptor.Descriptor(
    name='SaveCommand',
    full_name='SaveCommand',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
        _descriptor.FieldDescriptor(
            name='filename', full_name='SaveCommand.filename', index=0,
            number=1, type=9, cpp_type=9, label=1,
            has_default_value=False, default_value=b"".decode('utf-8'),
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
    ],
    serialized_start=235,
    serialized_end=266,
)

_LOADCOMMAND = _descriptor.Descriptor(
    name='LoadCommand',
    full_name='LoadCommand',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
        _descriptor.FieldDescriptor(
            name='filename', full_name='LoadCommand.filename', index=0,
            number=1, type=9, cpp_type=9, label=1,
            has_default_value=False, default_value=b"".decode('utf-8'),
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
    ],
    serialized_start=268,
    serialized_end=299,
)

_COMMAND = _descriptor.Descriptor(
    name='Command',
    full_name='Command',
    filename=None,
    file=DESCRIPTOR,
    containing_type=None,
    create_key=_descriptor._internal_create_key,
    fields=[
        _descriptor.FieldDescriptor(
            name='init', full_name='Command.init', index=0,
            number=1, type=11, cpp_type=10, label=1,
            has_default_value=False, default_value=None,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
        _descriptor.FieldDescriptor(
            name='predict', full_name='Command.predict', index=1,
            number=2, type=11, cpp_type=10, label=1,
            has_default_value=False, default_value=None,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
        _descriptor.FieldDescriptor(
            name='train', full_name='Command.train', index=2,
            number=3, type=11, cpp_type=10, label=1,
            has_default_value=False, default_value=None,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
        _descriptor.FieldDescriptor(
            name='valid', full_name='Command.valid', index=3,
            number=4, type=11, cpp_type=10, label=1,
            has_default_value=False, default_value=None,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
        _descriptor.FieldDescriptor(
            name='save', full_name='Command.save', index=4,
            number=5, type=11, cpp_type=10, label=1,
            has_default_value=False, default_value=None,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
        _descriptor.FieldDescriptor(
            name='load', full_name='Command.load', index=5,
            number=6, type=11, cpp_type=10, label=1,
            has_default_value=False, default_value=None,
            message_type=None, enum_type=None, containing_type=None,
            is_extension=False, extension_scope=None,
            serialized_options=None, file=DESCRIPTOR, create_key=_descriptor._internal_create_key),
    ],
    extensions=[
    ],
    nested_types=[],
    enum_types=[
    ],
    serialized_options=None,
    is_extendable=False,
    syntax='proto3',
    extension_ranges=[],
    oneofs=[
        _descriptor.OneofDescriptor(
            name='body', full_name='Command.body',
            index=0, containing_type=None,
            create_key=_descriptor._internal_create_key,
            fields=[]),
    ],
    serialized_start=302,
    serialized_end=509,
)

_PREDICTCOMMAND.fields_by_name['choicePoints'].message_type = ChoicePoint__pb2._CHOICEPOINT
_TRAINCOMMAND.fields_by_name['datapoints'].message_type = DataPoint__pb2._DATAPOINT
_VALIDCOMMAND.fields_by_name['datapoints'].message_type = DataPoint__pb2._DATAPOINT
_COMMAND.fields_by_name['init'].message_type = _INITCOMMAND
_COMMAND.fields_by_name['predict'].message_type = _PREDICTCOMMAND
_COMMAND.fields_by_name['train'].message_type = _TRAINCOMMAND
_COMMAND.fields_by_name['valid'].message_type = _VALIDCOMMAND
_COMMAND.fields_by_name['save'].message_type = _SAVECOMMAND
_COMMAND.fields_by_name['load'].message_type = _LOADCOMMAND
_COMMAND.oneofs_by_name['body'].fields.append(
    _COMMAND.fields_by_name['init'])
_COMMAND.fields_by_name['init'].containing_oneof = _COMMAND.oneofs_by_name['body']
_COMMAND.oneofs_by_name['body'].fields.append(
    _COMMAND.fields_by_name['predict'])
_COMMAND.fields_by_name['predict'].containing_oneof = _COMMAND.oneofs_by_name['body']
_COMMAND.oneofs_by_name['body'].fields.append(
    _COMMAND.fields_by_name['train'])
_COMMAND.fields_by_name['train'].containing_oneof = _COMMAND.oneofs_by_name['body']
_COMMAND.oneofs_by_name['body'].fields.append(
    _COMMAND.fields_by_name['valid'])
_COMMAND.fields_by_name['valid'].containing_oneof = _COMMAND.oneofs_by_name['body']
_COMMAND.oneofs_by_name['body'].fields.append(
    _COMMAND.fields_by_name['save'])
_COMMAND.fields_by_name['save'].containing_oneof = _COMMAND.oneofs_by_name['body']
_COMMAND.oneofs_by_name['body'].fields.append(
    _COMMAND.fields_by_name['load'])
_COMMAND.fields_by_name['load'].containing_oneof = _COMMAND.oneofs_by_name['body']
DESCRIPTOR.message_types_by_name['InitCommand'] = _INITCOMMAND
DESCRIPTOR.message_types_by_name['PredictCommand'] = _PREDICTCOMMAND
DESCRIPTOR.message_types_by_name['TrainCommand'] = _TRAINCOMMAND
DESCRIPTOR.message_types_by_name['ValidCommand'] = _VALIDCOMMAND
DESCRIPTOR.message_types_by_name['SaveCommand'] = _SAVECOMMAND
DESCRIPTOR.message_types_by_name['LoadCommand'] = _LOADCOMMAND
DESCRIPTOR.message_types_by_name['Command'] = _COMMAND
_sym_db.RegisterFileDescriptor(DESCRIPTOR)

InitCommand = _reflection.GeneratedProtocolMessageType('InitCommand', (_message.Message,), {
    'DESCRIPTOR': _INITCOMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:InitCommand)
})
_sym_db.RegisterMessage(InitCommand)

PredictCommand = _reflection.GeneratedProtocolMessageType('PredictCommand', (_message.Message,), {
    'DESCRIPTOR': _PREDICTCOMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:PredictCommand)
})
_sym_db.RegisterMessage(PredictCommand)

TrainCommand = _reflection.GeneratedProtocolMessageType('TrainCommand', (_message.Message,), {
    'DESCRIPTOR': _TRAINCOMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:TrainCommand)
})
_sym_db.RegisterMessage(TrainCommand)

ValidCommand = _reflection.GeneratedProtocolMessageType('ValidCommand', (_message.Message,), {
    'DESCRIPTOR': _VALIDCOMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:ValidCommand)
})
_sym_db.RegisterMessage(ValidCommand)

SaveCommand = _reflection.GeneratedProtocolMessageType('SaveCommand', (_message.Message,), {
    'DESCRIPTOR': _SAVECOMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:SaveCommand)
})
_sym_db.RegisterMessage(SaveCommand)

LoadCommand = _reflection.GeneratedProtocolMessageType('LoadCommand', (_message.Message,), {
    'DESCRIPTOR': _LOADCOMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:LoadCommand)
})
_sym_db.RegisterMessage(LoadCommand)

Command = _reflection.GeneratedProtocolMessageType('Command', (_message.Message,), {
    'DESCRIPTOR': _COMMAND,
    '__module__': 'Command_pb2'
    # @@protoc_insertion_point(class_scope:Command)
})
_sym_db.RegisterMessage(Command)

# @@protoc_insertion_point(module_scope)
