void cap(const struct Chord* self) {
  switch (*self->state) {
    case ACTIVATED:
      tap_key(self->value1);
      tap_key(self->value2);
      break;
    case DEACTIVATED:
      current_pseudolayer = CAPS;
      *self->state = IN_ONE_SHOT;
      break;
    case FINISHED:
    case PRESS_FROM_ACTIVE:
      current_pseudolayer = self->value1;
      a_key_went_through = false;
      break;
    case RESTART:
      if (a_key_went_through) {
        current_pseudolayer = self->pseudolayer;
      } else {
        *self->state = IN_ONE_SHOT;
      }
    default:
      break;
  }
}
void cap(const struct Chord* self) {\n  switch (*self->state) {\n    case ACTIVATED:\n      tap_key(self->value1);\n      tap_key(self->value2);\n      break;\n    case DEACTIVATED:\n      current_pseudolayer = CAPS;\n      *self->state = IN_ONE_SHOT;\n      break;\n    case FINISHED:\n    case PRESS_FROM_ACTIVE:\n      current_pseudolayer = self->value1;\n      a_key_went_through = false;\n      break;\n    case RESTART:\n      if (a_key_went_through) {\n        current_pseudolayer = self->pseudolayer;\n      } else {\n        *self->state = IN_ONE_SHOT;\n      }\n    default:\n      break;\n  }\n}\n
