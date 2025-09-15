# -Christmas-Promotion-Campaign-2024-Data-Analysis-Project
This project was conducted as part of the Digital Marketing Intelligence course at the University of Groningen. It analyzes customer engagement and business impact of a Christmas promotion campaign (2024) and provides actionable recommendations.

## 📊 Project Overview

| Aspect        | Description |
|---------------|-------------|
| **Context**   | From December 3rd to December 24th, 2024, a Christmas promotion campaign was launched in the app. Customers could open daily cards to receive coupons, win chances, and seasonal tips. A **streak system** rewarded consecutive daily participation. |
| **Objective** | To evaluate the success of the campaign in terms of participation, coupon usage, customer engagement, and sales impact, and to suggest improvements for future campaigns. |
| **Dataset**   | Provided Excel file `rug_wannagive_case.xlsx` with five sheets: campaign logs, coupon logs, transaction data, customer info, and campaign background. |
| **Methods**   | Data cleaning & preprocessing (R, dplyr, ggplot2), segmentation (K-Means & Hierarchical clustering), visualization, and statistical modeling (GLM). |

---

## 🔑 Key Findings

| Metric | Result |
|--------|--------|
| **Unique participants** | 7,764 customers joined the campaign |
| **Open rate** | 0.72, with stable engagement throughout the event |
| **Largest drop** | December 7th (Sinterklaas weekend), explained by reduced app activity |
| **Claim rate** | Coupons had a relatively low claim rate (39.2%), while Win cards performed much better (88.4%) |
| **Order peaks** | December 18th and 22nd showed the highest order volumes, likely due to broader seasonal discounts |
| **Ultimate streak** | Only 59 customers (<1%) achieved the full 22-day streak |
| **Customer segments** | Women aged 30–44 were the most engaged; “High-Value Buyers” cluster contributed disproportionately to revenue |
| **Coupon usage** | Customers completing the streak preferred high-value coupons (€20 off €80, €25 off €100) |

---

## 💡 Recommendations

- **Boost retention & streak completion**: introduce smaller milestones (e.g., 3-, 7-, 14-day rewards), personalized reminders, and “second-chance” mechanics.  
- **Improve coupon effectiveness**: diversify between low-threshold and high-value discounts, and personalize offers based on purchase history.  
- **Target high-value clusters**: provide VIP experiences and tailored rewards to strengthen loyalty.  
- **Benchmark campaigns**: compare with previous years or competitors to isolate true campaign effects.  

---

## 🛠️ Tools & Code

All analysis was performed in **R**. The workflow includes:  
- Data import & cleaning (`dplyr`, `readxl`)  
- Exploratory analysis & visualization (`ggplot2`)  
- Customer segmentation (K-Means, Hierarchical clustering)  
- Impact analysis (GLM models for order frequency, items, and revenue)  
