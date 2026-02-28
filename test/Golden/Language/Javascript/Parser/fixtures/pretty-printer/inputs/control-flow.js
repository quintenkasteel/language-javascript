// Control flow statements for round-trip testing
if (x > 0) {
    y = 1;
} else {
    y = 2;
}

for (var i = 0; i < 10; i++) {
    console.log(i);
}

while (running) {
    process();
}

try {
    riskyCall();
} catch (e) {
    handleError(e);
}
