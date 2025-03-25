let confettiCount = 0;
const maxConfetti = 100;
let confettiInterval;

function createConfetti() {
  // Limit the total number of confetti elements
  if (confettiCount >= maxConfetti) {
    return;
  }

  const confetti = document.createElement('div');
  confetti.classList.add('confetti');
  confetti.style.left = Math.random() * 100 + 'vw';
  confetti.style.backgroundColor = `hsl(${Math.random() * 360}, 100%, 50%)`;
  document.querySelector('.confetti-container').appendChild(confetti);

  confettiCount++;

  confetti.addEventListener('animationend', () => {
    confetti.remove();
    confettiCount--;
  });
}

function startConfetti() {
  // Create initial burst of confetti
  for (let i = 0; i < 50; i++) {
    setTimeout(createConfetti, i * 50);
  }

  // Continue creating confetti for a limited time
  confettiInterval = setInterval(createConfetti, 200);

  // Stop creating new confetti after 10 seconds
  setTimeout(() => {
    clearInterval(confettiInterval);
    // After 15 seconds, clear any remaining confetti
    setTimeout(() => {
      const container = document.querySelector('.confetti-container');
      if (container) {
        container.innerHTML = '';
      }
      confettiCount = 0;
    }, 5000);
  }, 10000);
}

// Start the confetti when the script loads if there's a container
document.addEventListener('DOMContentLoaded', () => {
  if (document.querySelector('.confetti-container')) {
    startConfetti();
  }
});
