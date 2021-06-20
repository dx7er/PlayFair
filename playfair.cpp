#include <iostream>
#include <string>
#include <cstdlib>
using namespace std;

struct COORD {
    int row;
    int col;
};

/*
 * PROTOTYPE Functions
 */
string onlyUnique(string);
string onlyAlphabet(string);
bool isRepeated(string, char);
bool findCh(string, char);
void printArray();
void fillList(string);
COORD getCoord(char);
string EnDeCryptText(string);
void getUserInput(string&, string&);

const int row = 5;
const int col = 5;
char listArray[row][col];

int main() {
    string text, key, textProccesed, keyProccesed;

    getUserInput(text, key); // Take input from user

    textProccesed = onlyAlphabet(text);
    keyProccesed = onlyUnique(key);

    fillList(keyProccesed); // Create table for encryption

    // Same function encrypt and decrypt text
    string encryptedText = EnDeCryptText(textProccesed);
    string decryptedText = EnDeCryptText(encryptedText);

    // Printing
    cout << endl
        << "Text: " << text << endl
        << "Key: " << key << endl << endl
        << "Remove Repeated from Key: " << keyProccesed << endl
        << "Only Alphabet: " << textProccesed << endl << endl
        << "Encrypted Text: " << encryptedText << endl
        << "Decrypted Text: " << decryptedText << endl
        << endl << endl
        << "Check: Text and decrypted text is " << ((textProccesed == decryptedText) ? "same" : "not same") << "."
        << endl << endl;


    // Printing encrypting and decrypting table.
    cout << endl << "Encrypting and Decrypting Table: " << endl;
    printArray();

    system("pause");
    return 0;
}

/*
 * Take valid input from user.
 */
void getUserInput(string& text, string& key) {
    do {
        cout << "Enter string to encrypt: ";
        getline(cin, text);
        string sampleText = onlyAlphabet(text);
        if (sampleText.size() % 2 != 0) {
            cout << "Error: No of alphabet must be in even. TRY AGAIN" << endl << endl;
            continue;
        }
        else {
            break;
        }
    } while (true);

    do {
        cout << "Enter key for encryption: ";
        getline(cin, key);
        if (key.size() > 25) {
            cout << "Error: Maximun unique character allowed is 25. TRY AGAIN" << endl;
            continue;
        }
        else {
            break;
        }
    } while (true);
}

/*
 * Encrypt text and decrypt text
 */
string EnDeCryptText(string text) {
    COORD pos1, pos2;
    string encryptText;
    for (int i = 0; i < (int)text.size(); i += 2) {
        pos1 = getCoord(text.c_str()[i]);
        pos2 = getCoord(text.c_str()[i + 1]);
        encryptText += listArray[pos2.row][pos1.col];
        encryptText += listArray[pos1.row][pos2.col];
    }
    return encryptText;
}

/*
 * Return coordination of pin location in listArray
 */
COORD getCoord(char pin) {
    COORD pos = { 0,0 };
    for (int r = 0; r < row; r++) {
        for (int c = 0; c < col; c++) {
            if (listArray[r][c] == pin) {
                pos.row = r;
                pos.col = c;
                return pos;
            }
        }
    }
    return pos;
}

/*
 * Add key and alphabet logic to array.
 */
void fillList(string key) {
    int firstIndex = 0;
    int initialsForNextLoop[2];
    char ch = 'a';
    for (int r = 0; r < row; r++) {
        for (int c = 0; c < col; c++) {
            if ((r * 5) + c < (int)key.size()) {
                initialsForNextLoop[0] = row;
                initialsForNextLoop[1] = col;
                listArray[r][c] = key.c_str()[firstIndex++];
            }
            else {
                if (ch <= 'z') {
                    while (findCh(key, ch)) ch++;
                    if (ch == 'j') ch++;
                    listArray[r][c] = ch++;
                }
            }
        }
    }
}

/*
 * Find character in string
 * TRUE if found
 */
bool findCh(string ch, char pin) {

    int size = ch.size();
    while (size--) {
        if (ch[size] == pin) {
            return true;
        }
    }
    return false;
}

/*
 * Tell whether specific character is repeated in string or not
 * TRUE if repeated
 */
bool isRepeated(string ch, char pin) {
    int size = ch.size();
    int timesFound = 0;
    while (size--) {
        if (pin == ch[size]) {
            timesFound++;
        }
    }
    return (timesFound > 1 ? true : false);
}

/*
 * Keep a-z and remove all other characters\
 */
string onlyAlphabet(string text) {
    for (int i = 0; i < (int)text.size(); i++) {
        if (!(
            (text.c_str()[i] >= 'a' && text.c_str()[i] <= 'z')
            ))
        {
            text.erase(i, 1);
            i--;
        }
        if (text.c_str()[i] == 'j') {
            text.replace(text.find(text.c_str()[i]), 1, "i");
        }
    }
    return text;
}

/*
 * Remove any repeating alphabet from string
 */
string onlyUnique(string key) {
    int size = key.size();
    while (size--) {
        if (isRepeated(key, key.c_str()[size]) || (key.c_str()[size] >= 'A' && key.c_str()[size] <= 'Z')) {
            key.erase(size, 1);
            size--;
        }

    }
    return onlyAlphabet(key);
}

/*
 * Print listArray to screen
 */
void printArray() {
    for (int r = 0; r < row; r++) {
        for (int c = 0; c < col; c++) {
            cout << listArray[r][c] << "\t";
        }
        cout << endl;
    }
}
