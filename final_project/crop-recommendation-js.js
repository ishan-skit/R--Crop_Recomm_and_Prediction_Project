// Theme management and additional features for Crop Recommendation System
document.addEventListener('DOMContentLoaded', function() {
    // Theme Management
    const themes = {
        light: {
            primary: '#27ae60',
            secondary: '#2980b9',
            background: '#ffffff',
            text: '#2c3e50',
            cardBg: '#f5f6fa'
        },
        dark: {
            primary: '#2ecc71',
            secondary: '#3498db',
            background: '#2c3e50',
            text: '#ecf0f1',
            cardBg: '#34495e'
        }
    };

    // Theme Switcher
    function setTheme(themeName) {
        const root = document.documentElement;
        const theme = themes[themeName];
        
        Object.keys(theme).forEach(property => {
            root.style.setProperty(`--${property}`, theme[property]);
        });
        
        localStorage.setItem('theme', themeName);
    }

    // Weather API Integration
    async function fetchWeatherData(latitude, longitude) {
        const API_KEY = 'YOUR_WEATHER_API_KEY';
        try {
            const response = await fetch(
                `https://api.openweathermap.org/data/2.5/weather?lat=${latitude}&lon=${longitude}&appid=${API_KEY}&units=metric`
            );
            const data = await response.json();
            updateWeatherUI(data);
        } catch (error) {
            console.error('Error fetching weather data:', error);
        }
    }

    // Crop Calendar
    class CropCalendar {
        constructor() {
            this.events = [];
            this.currentDate = new Date();
        }

        addEvent(date, description) {
            this.events.push({ date, description });
            this.renderCalendar();
        }

        renderCalendar() {
            const calendarEl = document.getElementById('crop-calendar');
            if (!calendarEl) return;

            const calendar = new FullCalendar.Calendar(calendarEl, {
                initialView: 'dayGridMonth',
                events: this.events,
                headerToolbar: {
                    left: 'prev,next today',
                    center: 'title',
                    right: 'dayGridMonth,timeGridWeek,timeGridDay'
                }
            });

            calendar.render();
        }
    }

    // Soil Analysis Chart
    function initializeSoilAnalysisChart(data) {
        const ctx = document.getElementById('soilAnalysisChart').getContext('2d');
        new Chart(ctx, {
            type: 'radar',
            data: {
                labels: ['Nitrogen', 'Phosphorus', 'Potassium', 'pH', 'Moisture'],
                datasets: [{
                    label: 'Current Soil Composition',
                    data: data,
                    backgroundColor: 'rgba(39, 174, 96, 0.2)',
                    borderColor: 'rgba(39, 174, 96, 1)',
                    borderWidth: 1
                }]
            },
            options: {
                scales: {
                    r: {
                        beginAtZero: true,
                        max: 100
                    }
                }
            }
        });
    }

    // Crop Growth Tracker
    class CropGrowthTracker {
        constructor() {
            this.stages = ['Seeding', 'Germination', 'Vegetative', 'Flowering', 'Harvest'];
            this.currentStage = 0;
        }

        updateStage(stage) {
            this.currentStage = stage;
            this.updateProgressBar();
        }

        updateProgressBar() {
            const progressBar = document.querySelector('.growth-progress');
            if (progressBar) {
                const progress = (this.currentStage / (this.stages.length - 1)) * 100;
                progressBar.style.width = `${progress}%`;
            }
        }
    }

    // Pest Detection System
    class PestDetectionSystem {
        constructor() {
            this.pestDatabase = {
                'aphids': {
                    symptoms: ['Yellowing leaves', 'Stunted growth', 'Sticky residue'],
                    treatment: 'Natural neem oil spray or introducing ladybugs'
                },
                'whiteflies': {
                    symptoms: ['White insects under leaves', 'Yellowing', 'Weak plants'],
                    treatment: 'Yellow sticky traps and insecticidal soap'
                }
                // Add more pest entries
            };
        }

        detectPest(symptoms) {
            return Object.entries(this.pestDatabase).find(([pest, data]) => 
                symptoms.every(symptom => data.symptoms.includes(symptom))
            );
        }
    }

    // Yield Predictor
    class YieldPredictor {
        constructor() {
            this.model = null;
        }

        async loadModel() {
            try {
                this.model = await tf.loadLayersModel('path/to/your/model.json');
            } catch (error) {
                console.error('Error loading yield prediction model:', error);
            }
        }

        async predictYield(inputs) {
            if (!this.model) await this.loadModel();
            const tensorInputs = tf.tensor2d([inputs]);
            const prediction = this.model.predict(tensorInputs);
            return prediction.dataSync()[0];
        }
    }

    // Initialize Components
    const cropCalendar = new CropCalendar();
    const growthTracker = new CropGrowthTracker();
    const pestDetector = new PestDetectionSystem();
    const yieldPredictor = new YieldPredictor();

    // Event Listeners
    document.getElementById('theme-toggle')?.addEventListener('click', () => {
        const currentTheme = localStorage.getItem('theme') || 'light';
        setTheme(currentTheme === 'light' ? 'dark' : 'light');
    });

    // Initialize notifications
    function initializeNotifications() {
        if ('Notification' in window) {
            Notification.requestPermission();
        }
    }

    // Export functions for Shiny
    window.cropRecommendationSystem = {
        setTheme,
        fetchWeatherData,
        cropCalendar,
        growthTracker,
        pestDetector,
        yieldPredictor,
        initializeNotifications
    };
});
