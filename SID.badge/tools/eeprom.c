// gcc -std=c99 -O3 -Wall -o eeprom eeprom.c

#include <stdio.h>
#include <string.h>

static unsigned char hi[32768];

static int current   = 128;
static int available = sizeof(hi) - 128;
static int header    = 0;

static void process_file(char *name) {
	FILE *input = fopen(name, "rb");
	if (input) {
		fseek(input, 0, SEEK_END);
		long int length = ftell(input);
		fseek(input, 0, SEEK_SET);
		if (!ferror(input) && length <= available) {
			printf("%08X %08X: %s %ld\n", current, available, name, length);
			int consumed = fread(&hi[current], 1, length, input);
			if (consumed == length) {
				hi[header++] = current;
				hi[header++] = current >> 8;
				hi[header++] = length;
				hi[header++] = length >> 8;

				current   += length;
				available -= length;
			}
		}
		fclose(input);
	}
}

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define SLOTS (32)

int main(int argc, char **argv) {
	if (argc < 3) {
		fprintf(stderr, "usage: %s eeprom_file SID_0 [SID_1 [...]]\n", argv[0]);
		return 1;
	}
	memset(hi, -1, sizeof(hi));

	for (int idx = 2; idx < MIN(argc, SLOTS + 2); idx++)
		process_file(argv[idx]);

	if (header < SLOTS * 4) {
		hi[header++] = 0;
		hi[header++] = 0;
		hi[header++] = 0;
		hi[header++] = 0;
	}

	FILE *result = fopen(argv[1], "wb");
	if (result) {
		fwrite(hi, 1, sizeof(hi), result);
		fclose(result);
	}
	return 0;
}
